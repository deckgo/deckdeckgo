/* eslint-disable */
/* tslint:disable */
/**
 * This is an autogenerated file created by the Stencil compiler.
 * It contains typing information for all components that exist in this project.
 */
import { HTMLStencilElement, JSXBase } from "@stencil/core/internal";
import { MonacoEditorOptions } from "./types/options";
export namespace Components {
    interface DeckgoMonacoEditor {
        "options": MonacoEditorOptions;
        "save": () => Promise<string | undefined>;
    }
}
declare global {
    interface HTMLDeckgoMonacoEditorElement extends Components.DeckgoMonacoEditor, HTMLStencilElement {
    }
    var HTMLDeckgoMonacoEditorElement: {
        prototype: HTMLDeckgoMonacoEditorElement;
        new (): HTMLDeckgoMonacoEditorElement;
    };
    interface HTMLElementTagNameMap {
        "deckgo-monaco-editor": HTMLDeckgoMonacoEditorElement;
    }
}
declare namespace LocalJSX {
    interface DeckgoMonacoEditor {
        "options"?: MonacoEditorOptions;
    }
    interface IntrinsicElements {
        "deckgo-monaco-editor": DeckgoMonacoEditor;
    }
}
export { LocalJSX as JSX };
declare module "@stencil/core" {
    export namespace JSX {
        interface IntrinsicElements {
            "deckgo-monaco-editor": LocalJSX.DeckgoMonacoEditor & JSXBase.HTMLAttributes<HTMLDeckgoMonacoEditorElement>;
        }
    }
}
