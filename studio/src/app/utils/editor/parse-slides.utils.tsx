import {convertStyle, Slide, SlideScope, SlideTemplate} from '@deckdeckgo/editor';
import {ParseElementsUtils} from '@deckdeckgo/studio';
import {h, JSX} from '@stencil/core';
import {v4 as uuid} from 'uuid';
import {EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export class ParseSlidesUtils {
  static async parseSlide(slide: Slide, contentEditable: boolean, ignoreSlideId: boolean = false): Promise<JSX.IntrinsicElements> {
    if (!document || !slide || !slide.data || !slide.data.template) {
      return null;
    }

    const template: SlideTemplate | undefined = SlideTemplate[slide.data.template.toUpperCase()];
    const slideTag: string = template ? `deckgo-slide-${template.toLowerCase()}` : slide.data.template;

    return this.parseSlideElement(slide, slideTag, contentEditable, ignoreSlideId);
  }

  private static parseSlideElement(
    slide: Slide,
    slideTag: string,
    contentEditable: boolean,
    ignoreSlideId: boolean
  ): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      let content = undefined;

      // Create a div to parse back to JSX its children
      const div = document.createElement('div');

      if (slide.data.content && slide.data.content !== undefined) {
        div.innerHTML = slide.data.content;
        content = await ParseElementsUtils.parseElements(div, true, contentEditable);
      }

      const userAttributes = slide.data.attributes
        ? Object.keys(slide.data.attributes)?.reduce((acc, key: string) => {
            acc[key] = slide.data.attributes[key];
            return acc;
          }, {})
        : {};

      const defaultAttributes = await this.parseDefaultAttributes(slide);

      const attributes = {
        ...userAttributes,
        ...defaultAttributes,
        ...(slide.data.scope !== SlideScope.DEFAULT && {scope: slide.data.scope})
      };

      const SlideElement: string = slideTag;

      const result: JSX.IntrinsicElements = (
        <SlideElement key={uuid()} slide_id={ignoreSlideId ? undefined : slide.id} {...attributes}>
          {content}
        </SlideElement>
      );

      resolve(result);
    });
  }

  private static async parseDefaultAttributes(slide: Slide) {
    const defaultAttributes = {
      style: slide.data.attributes ? convertStyle(slide.data.attributes.style) : undefined,
      src: slide.data.attributes && slide.data.attributes.src ? slide.data.attributes.src : undefined,
      'custom-background':
        slide.data.attributes && slide.data.attributes.customBackground ? slide.data.attributes.customBackground : undefined,
      'img-src': slide.data.attributes && slide.data.attributes.imgSrc ? slide.data.attributes.imgSrc : undefined,
      'img-alt': slide.data.attributes && slide.data.attributes.imgAlt ? slide.data.attributes.imgAlt : undefined
    };

    if (slide.data.template === SlideTemplate.QRCODE) {
      defaultAttributes['content'] = slide.data.attributes && slide.data.attributes.content ? slide.data.attributes.content : undefined;
      defaultAttributes['custom-qrcode'] = slide.data.attributes && slide.data.attributes.content ? 'true' : undefined;
    }

    if (slide.data.template === SlideTemplate.CHART) {
      defaultAttributes['type'] = slide.data.attributes && slide.data.attributes.type ? slide.data.attributes.type : undefined;
      defaultAttributes['inner-radius'] =
        slide.data.attributes && slide.data.attributes.innerRadius ? slide.data.attributes.innerRadius : undefined;
      defaultAttributes['animation'] =
        slide.data.attributes && slide.data.attributes.animation ? slide.data.attributes.animation : undefined;
      defaultAttributes['date-pattern'] =
        slide.data.attributes && slide.data.attributes.datePattern ? slide.data.attributes.datePattern : undefined;
      defaultAttributes['y-axis-domain'] =
        slide.data.attributes && slide.data.attributes.yAxisDomain ? slide.data.attributes.yAxisDomain : undefined;
      defaultAttributes['smooth'] = slide.data.attributes && slide.data.attributes.smooth === false ? 'false' : undefined;
      defaultAttributes['area'] = slide.data.attributes && slide.data.attributes.area === false ? 'false' : undefined;
      defaultAttributes['ticks'] = slide.data.attributes && slide.data.attributes.ticks ? slide.data.attributes.ticks : undefined;
      defaultAttributes['grid'] = slide.data.attributes && slide.data.attributes.grid ? 'true' : undefined;
      defaultAttributes['separator'] =
        slide.data.attributes && slide.data.attributes.separator ? slide.data.attributes.separator : undefined;
      defaultAttributes['custom-loader'] = 'true';
    }

    if (slide.data.template === SlideTemplate.SPLIT) {
      defaultAttributes['vertical'] = slide.data.attributes && slide.data.attributes.vertical ? 'true' : undefined;
      defaultAttributes['type'] = slide.data.attributes && slide.data.attributes.type ? slide.data.attributes.type : undefined;
    }

    if (slide.data.template === SlideTemplate.POLL) {
      const {pollUrl, socketUrl}: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      defaultAttributes['pollLink'] = pollUrl;
      defaultAttributes['socketUrl'] = socketUrl;
    }

    if (slide.data.template === SlideTemplate.AUTHOR) {
      defaultAttributes['img-mode'] = slide.data.attributes && slide.data.attributes.imgMode ? slide.data.attributes.imgMode : undefined;
    }

    if (slide.data.template === SlideTemplate['ASPECT-RATIO']) {
      defaultAttributes['grid'] = slide.data.attributes && slide.data.attributes.grid === false ? 'false' : 'true';
      defaultAttributes['editable'] = true;
    }

    if (slide.data.template === SlideTemplate.PLAYGROUND) {
      defaultAttributes['theme'] = slide.data.attributes && slide.data.attributes.theme ? slide.data.attributes.theme : undefined;
    }
    return defaultAttributes;
  }
}
