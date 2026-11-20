import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Text, wrapTextWithAnsi } from "@earendil-works/pi-tui";

class HangingIndentText extends Text {
	private content = "";
	private cachedWidth: number | undefined;
	private cachedLines: string[] | undefined;

	override setText(text: string): void {
		super.setText(text);
		this.content = text;
		this.cachedLines = undefined;
	}

	override invalidate(): void {
		super.invalidate();
		this.cachedLines = undefined;
	}

	override render(width: number): string[] {
		if (this.cachedLines && this.cachedWidth === width) return this.cachedLines;
		const lines: string[] = [];
		for (const paragraph of this.content.split("\n")) {
			const leadingSpaces = paragraph.match(/^ */)?.[0].length ?? 0;
			const indent = " ".repeat(Math.min(leadingSpaces, Math.max(0, width - 3)));
			const visibleText = paragraph.slice(leadingSpaces).replace(/\x1b\[[0-9;]*m/g, "");
			const operatorPrefix = visibleText.match(/^(?:&&|\|\||\|&|\|)[ \t]+/);
			const continuationSpaces = leadingSpaces + (operatorPrefix ? operatorPrefix[0].length - 1 : 2);
			const continuationIndent = " ".repeat(Math.min(continuationSpaces, Math.max(0, width - 1)));
			const wrapped = wrapTextWithAnsi(paragraph.slice(leadingSpaces), Math.max(1, width - continuationIndent.length));
			lines.push(indent + (wrapped[0] ?? ""));
			for (const continuation of wrapped.slice(1)) lines.push(continuationIndent + continuation);
		}
		this.cachedWidth = width;
		this.cachedLines = lines;
		return lines;
	}
}

function formatCommand(command: string, inlineHeredoc = false): string {
	const lines: { text: string; indent: number }[] = [];
	let start = 0;
	let commandIndent = 0;
	let redirection = false;
	let quote: "'" | '"' | "`" | undefined;
	let escaped = false;
	let parenDepth = 0;
	const quotedSubstitutionDepths: number[] = [];
	let parameterDepth = 0;
	const groupIndents: number[] = [];
	const baseIndent = () => groupIndents.length > 0 ? groupIndents[groupIndents.length - 1]! + 2 : 0;
	const segmentIndent = () => redirection ? Math.max(commandIndent, baseIndent() + 3) : commandIndent;

	const assignments: string[] = [];
	let assignmentEnd = 0;
	while (true) {
		const assignment = command.slice(assignmentEnd).match(/^[ \t]*([A-Za-z_][A-Za-z0-9_]*=(?:'[^']*'|"[^"]*"|[A-Za-z0-9_./~:-]*))(?=[ \t\r\n])/);
		if (!assignment) break;
		assignments.push(assignment[1]);
		assignmentEnd += assignment[0].length;
	}
	const afterAssignments = command.slice(assignmentEnd).trimStart();
	if (assignments.length > 0 && afterAssignments && !/^(?:[A-Za-z_][A-Za-z0-9_]*=|[;&|<>#])/.test(afterAssignments)) {
		for (const assignment of assignments) lines.push({ text: assignment, indent: 0 });
		start = assignmentEnd;
		commandIndent = 2;
	}

	for (let i = start; i < command.length; i++) {
		const char = command[i];
		if (escaped) {
			escaped = false;
			continue;
		}
		if (char === "\\" && quote !== "'") {
			escaped = true;
			continue;
		}
		if (quote === '"' && char === "$" && command[i + 1] === "(") {
			quotedSubstitutionDepths.push(parenDepth);
			quote = undefined;
			parenDepth++;
			i++;
			continue;
		}
		if (quote) {
			if (char === quote) quote = undefined;
			continue;
		}
		if (char === "'" || char === '"' || char === "`") {
			quote = char;
			continue;
		}
		if (char === "$" && command[i + 1] === "{") {
			parameterDepth++;
			i++;
			continue;
		}
		if (char === "}" && parameterDepth > 0) {
			parameterDepth--;
			continue;
		}
		if (char === "(") {
			parenDepth++;
			continue;
		}
		if (char === ")" && parenDepth > 0) {
			parenDepth--;
			if (quotedSubstitutionDepths.at(-1) === parenDepth) {
				quotedSubstitutionDepths.pop();
				quote = '"';
			}
			continue;
		}
		if (parenDepth > 0 || parameterDepth > 0) continue;

		if (char === "{" && /\s/.test(command[i + 1] ?? "")) {
			const beforeBrace = command.slice(start, i).trim();
			if (["", "&&", "||", "|", "|&"].includes(beforeBrace)) {
				groupIndents.push(commandIndent);
				continue;
			}
		}
		if (char === "}" && groupIndents.length > 0 && /[\s;]/.test(command[i - 1] ?? "")) {
			const before = command.slice(start, i).trim();
			if (before) lines.push({ text: before, indent: segmentIndent() });
			commandIndent = groupIndents.pop()!;
			const endsStatement = command[i + 1] === ";";
			lines.push({ text: endsStatement ? "};" : "}", indent: commandIndent });
			start = i + (endsStatement ? 2 : 1);
			redirection = false;
			if (endsStatement) {
				commandIndent = groupIndents.length > 0 ? groupIndents[groupIndents.length - 1]! + 2 : 0;
				i++;
			}
			continue;
		}

		if (char === "<" && command[i + 1] === "<" && command[i + 2] !== "<") {
			if (inlineHeredoc) {
				i++;
				continue;
			}
			const opener = command.slice(i).match(/^<<(-?)[ \t]*(?:'([^'\n]+)'|"([^"\n]+)"|([A-Za-z_][\w-]*))/);
			if (!opener) return command;
			// A prefix match is not the delimiter of a longer shell word.
			if (/[^\s;&|<>()]/.test(command[i + opener[0].length] ?? "")) return command;
			const delimiter = opener[2] ?? opener[3] ?? opener[4]!;
			const headerEnd = command.indexOf("\n", i + opener[0].length);
			if (headerEnd < 0 || command.slice(i + opener[0].length, headerEnd).includes("<<")) return command;
			let terminatorStart = headerEnd + 1;
			let terminatorEnd = terminatorStart;
			while (terminatorStart < command.length) {
				const newline = command.indexOf("\n", terminatorStart);
				terminatorEnd = newline < 0 ? command.length : newline;
				const line = command.slice(terminatorStart, terminatorEnd);
				if ((opener[1] ? line.replace(/^\t+/, "") : line) === delimiter) break;
				terminatorStart = terminatorEnd + 1;
			}
			if (terminatorStart >= command.length) return command;
			// Indent the embedded language once without changing its relative indentation or terminator.
			const header = formatCommand(command.slice(0, headerEnd), true);
			const indent = header.slice(header.lastIndexOf("\n") + 1).match(/^ */)?.[0] ?? "";
			const body = command.slice(headerEnd + 1, terminatorStart).replace(/^(.+)$/gm, `${indent}  $1`);
			const rest = command.slice(terminatorEnd + 1);
			return `${header}\n${body}${command.slice(terminatorStart, terminatorEnd)}${rest ? `\n${formatCommand(rest)}` : ""}`;
		}

		const redirect = char === "<" || char === ">" || (char === "&" && command[i + 1] === ">");
		if (redirect) {
			if ((char === "<" || char === ">") && (command[i - 1] === "<" || command[i - 1] === ">")) continue;
			let redirectStart = char === ">" && command[i - 1] === "&" ? i - 1 : i;
			if (char === "<" || (char === ">" && command[i - 1] !== "&")) {
				let fdStart = i - 1;
				while (fdStart >= start && /[0-9]/.test(command[fdStart] ?? "")) fdStart--;
				if (fdStart < i - 1 && (fdStart < start || /[\s;&|]/.test(command[fdStart] ?? ""))) {
					redirectStart = fdStart + 1;
				}
			}
			if (redirectStart > start) {
				const before = command.slice(start, redirectStart).trim();
				if (before) lines.push({ text: before, indent: segmentIndent() });
				start = redirectStart;
			}
			redirection = true;
			continue;
		}

		const and = char === "&" && command[i + 1] === "&";
		const or = char === "|" && command[i + 1] === "|";
		const semicolon = char === ";" && command[i - 1] !== ";" && command[i + 1] !== ";" && command[i + 1] !== "&";
		const pipe = char === "|" && command[i - 1] !== "|" && command[i - 1] !== ">" && command[i + 1] !== "|";
		if (pipe || and || or) {
			const before = command.slice(start, i).trim();
			if (before) lines.push({ text: before, indent: segmentIndent() });
			start = i;
			commandIndent = baseIndent() + 2;
			redirection = false;
			if (and || or || command[i + 1] === "&") i++;
			continue;
		}
		if (!semicolon) continue;
		const keyword = command.slice(i + 1).match(/^([ \t\r\n]+)(?:then|do)\b/);
		if (keyword) {
			command = command.slice(0, i + 1) + " " + command.slice(i + 1 + keyword[1].length);
			continue;
		}
		lines.push({ text: command.slice(start, i + 1).trim(), indent: segmentIndent() });
		start = i + 1;
		commandIndent = groupIndents.length > 0 ? groupIndents[groupIndents.length - 1]! + 2 : 0;
		redirection = false;
	}

	if (quote || escaped || parenDepth !== 0 || parameterDepth !== 0 || groupIndents.length > 0) return command;
	const rest = command.slice(start).trim();
	if (rest) lines.push({ text: rest, indent: segmentIndent() });
	return lines.map((line) => `${" ".repeat(line.indent)}${line.text}`).join("\n") || command;
}

export default function (pi: ExtensionAPI): void {
	pi.registerToolRenderer((toolName, next) => {
		const renderers = next();
		if (toolName !== "bash" || !renderers?.renderCall) return renderers;
		const renderCall = renderers.renderCall;
		return {
			...renderers,
			renderCall(args: unknown, theme, context) {
				const component = renderCall(args, theme, context);
				if (!(component instanceof Text) || typeof args !== "object" || args === null || !("command" in args) || typeof args.command !== "string") {
					return component;
				}
				const formatted = formatCommand(args.command);
				const timeout = "timeout" in args && typeof args.timeout === "number" ? args.timeout : undefined;
				const display = component instanceof HangingIndentText ? component : new HangingIndentText("", 0, 0);
				display.setText(
					formatted.split("\n").map((line) => {
						const indent = line.match(/^ */)?.[0] ?? "";
						return indent + theme.fg("toolTitle", theme.bold(line.slice(indent.length)));
					}).join("\n") +
						(timeout ? theme.fg("muted", ` (timeout ${timeout}s)`) : ""),
				);
				return display;
			},
		};
	});
}
