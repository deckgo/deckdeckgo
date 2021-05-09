import { ExecCommandAction, ExecCommandStyle } from "../interfaces/interfaces";
import { FontSize } from "../types/enums";

export const execCommandNative = (action: ExecCommandAction) => {
  if (action.cmd === 'style') {
    const detail: ExecCommandStyle = action.detail as ExecCommandStyle;

    // @ts-ignore
    document.execCommand("styleWithCSS", false, true);

    switch (detail.style) {
      case 'color':
        document.execCommand('foreColor', false, detail.value);
        break;
      case 'background-color' :
        document.execCommand('backColor', false, detail.value);
        break;
      case 'font-size':
        document.execCommand('fontSize', false, FontSize[detail.value.replace('-', '_').toUpperCase()]);
        break;
      case 'font-weight':
        document.execCommand('bold', false, null);
        break;
      case 'font-style':
        document.execCommand('italic', false, null);
        break;
      case 'text-decoration':
        document.execCommand(detail.value === 'line-through' ? 'strikeThrough' : 'underline', false, null);
        break;
    }
  }
}
