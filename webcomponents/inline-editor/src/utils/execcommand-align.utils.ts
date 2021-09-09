import {DeckdeckgoInlineEditorUtils} from './utils';
import {ContentAlign} from '../types/enums';

export const execCommandAlign = async (anchorEvent: MouseEvent | TouchEvent, containers: string, align: ContentAlign) => {
  const anchorElement: HTMLElement = anchorEvent.target as HTMLElement;
  const container: HTMLElement | undefined = await DeckdeckgoInlineEditorUtils.findContainer(containers, anchorElement);

  if (!container) {
    return;
  }

  container.style.textAlign = container?.style.textAlign === align ? '' : align;
};
