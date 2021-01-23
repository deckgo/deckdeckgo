import {SlideScope} from '../../models/data/slide';

export class SlideUtils {
  static isSlideTemplate(scope: SlideScope | undefined): boolean {
    return scope && scope !== undefined && (scope === SlideScope.COMMUNITY || scope === SlideScope.USER);
  }

  static slideScope(element: HTMLElement | undefined): SlideScope {
    return element?.hasAttribute('scope') ? <SlideScope>element.getAttribute('scope') : SlideScope.DEFAULT;
  }
}
