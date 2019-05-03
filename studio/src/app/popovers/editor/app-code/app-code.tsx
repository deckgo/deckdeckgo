import {Component, Element, EventEmitter, Prop, State} from '@stencil/core';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

enum CodeColorType {
    COMMENTS,
    PUNCTUATION,
    PROPERTY,
    SELECTOR,
    OPERATOR,
    KEYWORD,
    FUNCTION,
    REGEX
}

@Component({
    tag: 'app-code',
    styleUrl: 'app-code.scss'
})
export class AppCode {

    @Prop({connect: 'ion-alert-controller'}) alertController: HTMLIonAlertControllerElement;

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    codeDidChange: EventEmitter<HTMLElement>;

    private hidePopoverTimer: number;

    private prismService: PrismService;

    private currentLanguage: string = 'javascript';
    private languages: PrismLanguage[];

    @State()
    private codeColorType: CodeColorType = undefined;

    @State()
    private codeColor: string;

    @State()
    private highlightLines: string;

    @State()
    private highlightColor: string;

    constructor() {
        this.prismService = PrismService.getInstance();
    }

    async componentWillLoad() {
        this.languages = await this.prismService.getLanguages();

        await this.initCurrentLanguage();
        await this.initCurrentHiglight();
    }

    private initCurrentLanguage(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.currentLanguage = this.selectedElement && this.selectedElement.getAttribute('language') ? this.selectedElement.getAttribute('language') : 'javascript';
            this.codeColor = await this.initColor();

            resolve();
        });
    }

    private initCurrentHiglight(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.highlightLines = this.selectedElement && this.selectedElement.getAttribute('highlight-lines') ? this.selectedElement.getAttribute('highlight-lines') : null;
            this.highlightColor = this.selectedElement && this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-line-background') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-line-background') : '#3880ff';

            resolve();
        });
    }

    private selectColor($event, colorFunction: Function): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement || !this.selectedElement.parentElement) {
                resolve();
                return;
            }

            if (!$event || !$event.target || !$event.target.value) {
                resolve();
                return;
            }

            await this.privateHideShowPopover();

            colorFunction($event);

            this.emitCodeDidChange();

            resolve();
        });
    }

    private setCodeColor = ($event) => {
        this.codeColor = $event.target.value;
        this.selectedElement.style.setProperty(this.getStyle(), $event.target.value);
    };

    private setHighlightColor = ($event) => {
        this.highlightColor = $event.target.value;
        this.selectedElement.style.setProperty('--deckgo-highlight-code-line-background', $event.target.value);
    };

    private privateHideShowPopover(): Promise<void> {
        return new Promise<void>((resolve) => {
            const popover: HTMLIonPopoverElement = this.el.closest('ion-popover');

            popover.style.visibility = 'hidden';

            if (this.hidePopoverTimer) {
                clearTimeout(this.hidePopoverTimer);
            }

            this.hidePopoverTimer = setTimeout(() => {
                popover.style.visibility = 'initial';
            }, 1000);

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

    private toggleColorType($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            this.codeColorType = $event.detail.value;
            this.codeColor = await this.initColor();

            resolve();
        });
    }

    private getStyle(): string {
        if (this.codeColorType === CodeColorType.PUNCTUATION) {
            return '--deckgo-highlight-code-token-punctuation';
        } else if (this.codeColorType === CodeColorType.PROPERTY) {
            return '--deckgo-highlight-code-token-property';
        } else if (this.codeColorType === CodeColorType.SELECTOR) {
            return '--deckgo-highlight-code-token-selector';
        } else if (this.codeColorType === CodeColorType.OPERATOR) {
            return '--deckgo-highlight-code-token-operator';
        } else if (this.codeColorType === CodeColorType.KEYWORD) {
            return '--deckgo-highlight-code-token-atrule';
        } else if (this.codeColorType === CodeColorType.FUNCTION) {
            return '--deckgo-highlight-code-token-function';
        } else if (this.codeColorType === CodeColorType.REGEX) {
            return '--deckgo-highlight-code-token-regex';
        } else {
            return '--deckgo-highlight-code-token-comment';
        }
    }

    private initColor(): Promise<string> {
        return new Promise<string>((resolve) => {
            if (!this.selectedElement || !this.selectedElement.style) {
                resolve(null);
                return;
            }

            if (this.codeColorType === CodeColorType.PUNCTUATION) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-punctuation') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-punctuation') : '#708090');
            } else if (this.codeColorType === CodeColorType.PROPERTY) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-property') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-property') : '#990055');
            } else if (this.codeColorType === CodeColorType.SELECTOR) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-selector') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-selector') : '#669900');
            } else if (this.codeColorType === CodeColorType.OPERATOR) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-operator') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-operator') : '#9a6e3a');
            } else if (this.codeColorType === CodeColorType.KEYWORD) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-atrule') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-atrule') : '#0077AA');
            } else if (this.codeColorType === CodeColorType.FUNCTION) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-function') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-function') : '#DD4A68');
            } else if (this.codeColorType === CodeColorType.REGEX) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-regex') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-regex') : '#EE9900');
            } else {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-comment') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-comment') : '#999999');
            }
        });
    }

    private handleInput($event: CustomEvent<KeyboardEvent>) {
        this.highlightLines = ($event.target as InputTargetEvent).value;
    }

    private highlightSelectedLines(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            const currentHighlight: string = this.selectedElement.getAttribute('highlight-lines');

            if (currentHighlight === this.highlightLines) {
                resolve();
                return;
            }

            this.selectedElement.setAttribute('highlight-lines', this.highlightLines);

            // Reload component with new lines to highlight
            await (this.selectedElement as any).load();

            this.emitCodeDidChange();

            resolve();
        });
    }

    private async presentHighlightInfo() {
        const alert = await this.alertController.create({
            message: 'If you wish to highlight some specific lines of your code, list their line numbers separately using comma.<br/><br/>For example: 0,2 7,7 13,15<br/><br/>Which would highlight lines 0 to 2, line 7 and lines 13 to 15.',
            buttons: ['Ok']
        });

        return await alert.present();
    }

    render() {
        return [<div class="ion-padding"><h2>Code attributes</h2></div>,
            <ion-list>
                <ion-item-divider><ion-label>Language</ion-label></ion-item-divider>

                <ion-item class="select">
                    <ion-label>Language</ion-label>
                    <ion-select value={this.currentLanguage} onIonChange={(e: CustomEvent) => this.toggleCodeLanguage(e)} class="ion-padding-start ion-padding-end" interfaceOptions={{backdropDismiss: false}}>
                        {this.renderSelectOptions()}
                    </ion-select>
                </ion-item>

                <ion-item-divider class="ion-padding-top"><ion-label>Colors</ion-label></ion-item-divider>

                <ion-item class="select">
                    <ion-label>Apply color to</ion-label>

                    <ion-select value={this.codeColorType} placeholder="Select a category"
                                onIonChange={(e: CustomEvent) => this.toggleColorType(e)} class="ion-padding-start ion-padding-end">
                        <ion-select-option value={CodeColorType.COMMENTS}>Comments</ion-select-option>
                        <ion-select-option value={CodeColorType.FUNCTION}>Functions</ion-select-option>
                        <ion-select-option value={CodeColorType.KEYWORD}>Keywords</ion-select-option>
                        <ion-select-option value={CodeColorType.OPERATOR}>Operators</ion-select-option>
                        <ion-select-option value={CodeColorType.PUNCTUATION}>Punctuation</ion-select-option>
                        <ion-select-option value={CodeColorType.PROPERTY}>Properties</ion-select-option>
                        <ion-select-option value={CodeColorType.REGEX}>Regex</ion-select-option>
                        <ion-select-option value={CodeColorType.SELECTOR}>Selector</ion-select-option>
                    </ion-select>
                </ion-item>

                <ion-item disabled={this.codeColorType === undefined}>
                    <ion-label>Color</ion-label>
                    <input type="color" value={this.codeColor} onChange={(e) => this.selectColor(e, this.setCodeColor)}></input>
                </ion-item>

                <ion-item-divider class="ion-padding-top">
                    <ion-label>Highlight lines</ion-label>
                    <button slot="end" class="info" onClick={() => this.presentHighlightInfo()}>
                        <ion-icon name="help"></ion-icon>
                    </button>
                </ion-item-divider>

                <ion-item>
                    <ion-input value={this.highlightLines} placeholder="List your lines here" debounce={500}
                               onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
                                onIonChange={() => this.highlightSelectedLines()}></ion-input>
                </ion-item>

                <ion-item disabled={!this.highlightLines}>
                    <ion-label>Color</ion-label>
                    <input type="color" value={this.highlightColor} onChange={(e) => this.selectColor(e, this.setHighlightColor)}></input>
                </ion-item>
        </ion-list>]
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
