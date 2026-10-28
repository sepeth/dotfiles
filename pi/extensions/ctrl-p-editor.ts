import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { matchesKey } from "@earendil-works/pi-tui";

class HistoryAwareEditor extends CustomEditor {
  override handleInput(data: string): void {
    if (this.getText().length === 0) {
      super.handleInput(data);
      return;
    }

    if (matchesKey(data, "ctrl+p")) {
      // The editor's Up action browses history at the top, even with text present.
      const atTop = (this as unknown as { isOnFirstVisualLine(): boolean }).isOnFirstVisualLine();
      super.handleInput(atTop ? "\x01" : "\x1b[A");
      return;
    }

    if (matchesKey(data, "ctrl+n")) {
      // The editor's Down action browses history at the bottom of a history entry.
      const atBottom = (this as unknown as { isOnLastVisualLine(): boolean }).isOnLastVisualLine();
      super.handleInput(atBottom ? "\x05" : "\x1b[B");
      return;
    }

    super.handleInput(data);
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode === "tui") {
      ctx.ui.setEditorComponent((tui, theme, keybindings) => new HistoryAwareEditor(tui, theme, keybindings));
    }
  });
}
