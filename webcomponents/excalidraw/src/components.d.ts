/* eslint-disable */
/* tslint:disable */
/**
 * This is an autogenerated file created by the Stencil compiler.
 * It contains typing information for all components that exist in this project.
 */
import { HTMLStencilElement, JSXBase } from "@stencil/core/internal";
import { ExcalidrawScene } from "./excalidraw/excalidraw.app";
export namespace Components {
    interface DeckgoExcalidraw {
        /**
          * Export Excalidraw scene data to blob
         */
        "exportScene": () => Promise<Blob | null>;
        /**
          * An Excalidraw scene that contains app state and elements. On change, Excalidraw will be updated.
         */
        "scene": ExcalidrawScene;
        /**
          * Export Excalidraw scene to blob - i.e. to image
          * @param type The mime type of the image. Default: image/webp
         */
        "toBlob": (type?: string) => Promise<Blob>;
    }
}
declare global {
    interface HTMLDeckgoExcalidrawElement extends Components.DeckgoExcalidraw, HTMLStencilElement {
    }
    var HTMLDeckgoExcalidrawElement: {
        prototype: HTMLDeckgoExcalidrawElement;
        new (): HTMLDeckgoExcalidrawElement;
    };
    interface HTMLElementTagNameMap {
        "deckgo-excalidraw": HTMLDeckgoExcalidrawElement;
    }
}
declare namespace LocalJSX {
    interface DeckgoExcalidraw {
        /**
          * An Excalidraw scene that contains app state and elements. On change, Excalidraw will be updated.
         */
        "scene"?: ExcalidrawScene;
    }
    interface IntrinsicElements {
        "deckgo-excalidraw": DeckgoExcalidraw;
    }
}
export { LocalJSX as JSX };
declare module "@stencil/core" {
    export namespace JSX {
        interface IntrinsicElements {
            "deckgo-excalidraw": LocalJSX.DeckgoExcalidraw & JSXBase.HTMLAttributes<HTMLDeckgoExcalidrawElement>;
        }
    }
}
