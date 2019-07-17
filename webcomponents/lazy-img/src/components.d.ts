/* tslint:disable */
/**
 * This is an autogenerated file created by the Stencil compiler.
 * It contains typing information for all components that exist in this project.
 */


import { HTMLStencilElement, JSXBase } from '@stencil/core/internal';


export namespace Components {
  interface DeckgoLazyImg {
    'ariaLabel': string;
    'imgAlt': string;
    'imgErrorSrc': string;
    'imgSizes': string;
    'imgSrc': string;
    'imgSrcSet': string;
    'lazyLoad': () => Promise<void>;
    'observerRootMargin': string;
    'observerThreshold': number | number[];
    'svgSrc': string;
  }
}

declare global {


  interface HTMLDeckgoLazyImgElement extends Components.DeckgoLazyImg, HTMLStencilElement {}
  var HTMLDeckgoLazyImgElement: {
    prototype: HTMLDeckgoLazyImgElement;
    new (): HTMLDeckgoLazyImgElement;
  };
  interface HTMLElementTagNameMap {
    'deckgo-lazy-img': HTMLDeckgoLazyImgElement;
  }
}

declare namespace LocalJSX {
  interface DeckgoLazyImg extends JSXBase.HTMLAttributes<HTMLDeckgoLazyImgElement> {
    'ariaLabel'?: string;
    'imgAlt'?: string;
    'imgErrorSrc'?: string;
    'imgSizes'?: string;
    'imgSrc'?: string;
    'imgSrcSet'?: string;
    'observerRootMargin'?: string;
    'observerThreshold'?: number | number[];
    'onLazyImgDidLoad'?: (event: CustomEvent<any>) => void;
    'svgSrc'?: string;
  }

  interface IntrinsicElements {
    'deckgo-lazy-img': DeckgoLazyImg;
  }
}

export { LocalJSX as JSX };


declare module "@stencil/core" {
  export namespace JSX {
    interface IntrinsicElements extends LocalJSX.IntrinsicElements {}
  }
}


