import {ExecCommandAction, ExecCommandList, ExecCommandStyle} from '../interfaces/interfaces';

import {execCommandList} from './execcommand-list.utils';
import {execCommandStyle} from './execcommand-style.utils';

export function execCommand(selection: Selection, action: ExecCommandAction, containers: string) {
  if (!document || !selection) {
    return;
  }

  if (action.cmd === 'style') {
    execCommandStyle(selection, action.detail as ExecCommandStyle, containers);
  } else if (action.cmd === 'list') {
    execCommandList(selection, action.detail as ExecCommandList, containers);
  }
}
