/* tslint:disable */
/**
 * This is an autogenerated file created by the Stencil compiler.
 * It contains typing information for all components that exist in this project.
 */


import { HTMLStencilElement, JSXBase } from '@stencil/core/internal';
import {
  DeckdeckgoEvent,
  DeckdeckgoSlideDefinition,
} from '@deckdeckgo/types';
import {
  ConnectionState,
} from './services/communication/communication.service';


export namespace Components {
  interface DeckgoRemote {
    'autoConnect': boolean;
    'connect': () => Promise<void>;
    'deleteSlide': () => Promise<void>;
    'disconnect': () => Promise<void>;
    'height': number;
    'length': number;
    'moveDraw': (leftOffset: number, transitionDuration: string) => Promise<void>;
    'nextSlide': () => Promise<void>;
    'prevSlide': () => Promise<void>;
    'room': string;
    'server': string;
    'slideTo': (index: number, speed?: number) => Promise<void>;
    'slides': DeckdeckgoSlideDefinition[];
    'updateSlides': () => Promise<void>;
    'width': number;
  }
}

declare global {


  interface HTMLDeckgoRemoteElement extends Components.DeckgoRemote, HTMLStencilElement {}
  var HTMLDeckgoRemoteElement: {
    prototype: HTMLDeckgoRemoteElement;
    new (): HTMLDeckgoRemoteElement;
  };
  interface HTMLElementTagNameMap {
    'deckgo-remote': HTMLDeckgoRemoteElement;
  }
}

declare namespace LocalJSX {
  interface DeckgoRemote extends JSXBase.HTMLAttributes<HTMLDeckgoRemoteElement> {
    'autoConnect'?: boolean;
    'height'?: number;
    'length'?: number;
    'onEvent'?: (event: CustomEvent<DeckdeckgoEvent>) => void;
    'onState'?: (event: CustomEvent<ConnectionState>) => void;
    'room'?: string;
    'server'?: string;
    'slides'?: DeckdeckgoSlideDefinition[];
    'width'?: number;
  }

  interface IntrinsicElements {
    'deckgo-remote': DeckgoRemote;
  }
}

export { LocalJSX as JSX };


declare module "@stencil/core" {
  export namespace JSX {
    interface IntrinsicElements extends LocalJSX.IntrinsicElements {}
  }
}


