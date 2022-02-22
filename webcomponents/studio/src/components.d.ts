/* eslint-disable */
/* tslint:disable */
/**
 * This is an autogenerated file created by the Stencil compiler.
 * It contains typing information for all components that exist in this project.
 */
import { HTMLStencilElement, JSXBase } from "@stencil/core/internal";
import { StudioConfig } from "./types/config";
export namespace Components {
    interface DeckgoIndicator {
    }
    interface DeckgoStudio {
        "initNewDoc": () => Promise<void>;
        "studioConfig": StudioConfig;
    }
}
declare global {
    interface HTMLDeckgoIndicatorElement extends Components.DeckgoIndicator, HTMLStencilElement {
    }
    var HTMLDeckgoIndicatorElement: {
        prototype: HTMLDeckgoIndicatorElement;
        new (): HTMLDeckgoIndicatorElement;
    };
    interface HTMLDeckgoStudioElement extends Components.DeckgoStudio, HTMLStencilElement {
    }
    var HTMLDeckgoStudioElement: {
        prototype: HTMLDeckgoStudioElement;
        new (): HTMLDeckgoStudioElement;
    };
    interface HTMLElementTagNameMap {
        "deckgo-indicator": HTMLDeckgoIndicatorElement;
        "deckgo-studio": HTMLDeckgoStudioElement;
    }
}
declare namespace LocalJSX {
    interface DeckgoIndicator {
    }
    interface DeckgoStudio {
        "studioConfig"?: StudioConfig;
    }
    interface IntrinsicElements {
        "deckgo-indicator": DeckgoIndicator;
        "deckgo-studio": DeckgoStudio;
    }
}
export { LocalJSX as JSX };
declare module "@stencil/core" {
    export namespace JSX {
        interface IntrinsicElements {
            "deckgo-indicator": LocalJSX.DeckgoIndicator & JSXBase.HTMLAttributes<HTMLDeckgoIndicatorElement>;
            "deckgo-studio": LocalJSX.DeckgoStudio & JSXBase.HTMLAttributes<HTMLDeckgoStudioElement>;
        }
    }
}