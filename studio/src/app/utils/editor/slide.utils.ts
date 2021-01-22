import {SlideType} from '../../models/data/slide';

export class SlideUtils {
  static isSlideTemplate(type: SlideType | undefined): boolean {
    return type && type !== undefined && (type === SlideType.COMMUNITY || type === SlideType.USER);
  }
}
