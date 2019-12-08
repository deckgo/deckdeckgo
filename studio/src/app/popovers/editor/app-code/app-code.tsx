import {Component, Element, EventEmitter, Prop, State, h} from '@stencil/core';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

enum CodeFontSize {
    VERY_SMALL,
    SMALL,
    NORMAL,
    BIG,
    VERY_BIG
}

@Component({
    tag: 'app-code',
    styleUrl: 'app-code.scss'
})
export class AppCode {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    codeDidChange: EventEmitter<HTMLElement>;

    private prismService: PrismService;

    private currentLanguage: string = 'javascript';
    private languages: PrismLanguage[];

    @State()
    private currentFontSize: CodeFontSize = undefined;

    @State()
    private lineNumbers: boolean = false;

    constructor() {
        this.prismService = PrismService.getInstance();
    }

    async componentWillLoad() {
        this.languages = await this.prismService.getLanguages();

        await this.initCurrent();
    }

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
    }

    private initCurrent(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.currentLanguage = this.selectedElement && this.selectedElement.getAttribute('language') ? this.selectedElement.getAttribute('language') : 'javascript';
            this.currentFontSize = await this.initFontSize();
            this.lineNumbers = this.selectedElement && this.selectedElement.hasAttribute('line-numbers');

            resolve();
        });
    }

    private toggleCodeLanguage($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (!$event || !$event.detail || !$event.detail.value) {
                resolve();
                return;
            }

            const currentLanguage: string = this.selectedElement.getAttribute('language');

            if ($event.detail.value === currentLanguage) {
                resolve();
                return;
            }

            this.selectedElement.setAttribute('language', $event.detail.value);

            // Reload component with new language
            await (this.selectedElement as any).load();

            this.emitCodeDidChange();

            resolve();
        });
    }

    private emitCodeDidChange() {
        this.codeDidChange.emit(this.selectedElement);
    }

    private initFontSize(): Promise<CodeFontSize> {
        return new Promise<CodeFontSize>((resolve) => {
            if (!this.selectedElement || !this.selectedElement.style) {
                resolve(null);
                return;
            }

            const property: string = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-font-size');

            if (property === '50%') {
                resolve(CodeFontSize.VERY_SMALL);
            } else if (property === '75%') {
                resolve(CodeFontSize.SMALL);
            } else if (property === '150%') {
                resolve(CodeFontSize.BIG);
            } else if (property === '200%') {
                resolve(CodeFontSize.VERY_BIG);
            } else {
                resolve(CodeFontSize.NORMAL);
            }
        });
    }

    private toggleFontSize($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            this.currentFontSize = $event.detail.value;

            if (!this.selectedElement) {
                resolve();
                return;
            }

            this.selectedElement.style.removeProperty('--deckgo-highlight-code-font-size');

            if (this.currentFontSize === CodeFontSize.VERY_SMALL) {
                this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '50%');
            } else if (this.currentFontSize === CodeFontSize.SMALL) {
                this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '75%');
            } else if (this.currentFontSize === CodeFontSize.BIG) {
                this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '150%');
            } else if (this.currentFontSize === CodeFontSize.VERY_BIG) {
                this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '200%');
            }

            this.emitCodeDidChange();

            resolve();
        });
    }

    private toggleLineNumbers($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            this.selectedElement.setAttribute('line-numbers', $event.detail.checked);

            this.emitCodeDidChange();

            resolve();
        });
    }

    render() {
        return [<ion-toolbar>
            <h2>Code attributes</h2>
            <ion-router-link slot="end" onClick={() => this.closePopover()}>
                <ion-icon name="close"></ion-icon>
            </ion-router-link>
        </ion-toolbar>,
            <ion-list>
                <ion-item-divider>
                    <ion-label>Language</ion-label>
                </ion-item-divider>

                <ion-item class="select">
                    <ion-label>Language</ion-label>
                    <ion-select value={this.currentLanguage}
                                onIonChange={(e: CustomEvent) => this.toggleCodeLanguage(e)}
                                class="ion-padding-start ion-padding-end" interfaceOptions={{backdropDismiss: false}}>
                        {this.renderSelectOptions()}
                    </ion-select>
                </ion-item>

                <ion-item-divider class="ion-padding-top">
                    <ion-label>Font size</ion-label>
                </ion-item-divider>

                <ion-item class="select">
                    <ion-label>Size</ion-label>

                    <ion-select value={this.currentFontSize} placeholder="Select a font size"
                                onIonChange={(e: CustomEvent) => this.toggleFontSize(e)}
                                class="ion-padding-start ion-padding-end">
                        <ion-select-option value={CodeFontSize.VERY_SMALL}>Very small</ion-select-option>
                        <ion-select-option value={CodeFontSize.SMALL}>Small</ion-select-option>
                        <ion-select-option value={CodeFontSize.NORMAL}>Normal</ion-select-option>
                        <ion-select-option value={CodeFontSize.BIG}>Big</ion-select-option>
                        <ion-select-option value={CodeFontSize.VERY_BIG}>Very big</ion-select-option>
                    </ion-select>
                </ion-item>

                <ion-item-divider>
                    <ion-label>Lines</ion-label>
                </ion-item-divider>

                <ion-item>
                    <ion-label>Display line numbers</ion-label>
                    <ion-checkbox slot="end" checked={this.lineNumbers}
                                  onIonChange={($event: CustomEvent) => this.toggleLineNumbers($event)}></ion-checkbox>
                </ion-item>
            </ion-list>
        ]
    }

    private renderSelectOptions() {
        if (this.languages) {
            return (
                this.languages.map((language: PrismLanguage) => {
                    return <ion-select-option value={language.language}>{language.title}</ion-select-option>
                })
            );
        } else {
            return undefined;
        }
    }
}
