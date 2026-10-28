import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { matchesKey } from "@earendil-works/pi-tui";

class CtrlPEditor extends CustomEditor {
  override handleInput(data: string): void {
    if (!matchesKey(data, "ctrl+p") || this.getText().length === 0) {
      super.handleInput(data);
      return;
    }

    // The editor's Up action browses history at the top, even with text present.
    const atTop = (this as unknown as { isOnFirstVisualLine(): boolean }).isOnFirstVisualLine();
    super.handleInput(atTop ? "\x01" : "\x1b[A");
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode === "tui") {
      ctx.ui.setEditorComponent((tui, theme, keybindings) => new CtrlPEditor(tui, theme, keybindings));
    }
  });
}
