import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';
import {alertController} from '@ionic/core';

enum CodeColorType {
    COMMENTS,
    PUNCTUATION,
    PROPERTY,
    SELECTOR,
    OPERATOR,
    KEYWORD,
    FUNCTION,
    REGEX,
    LINE_NUMBERS
}

@Component({
    tag: 'app-color-code'
})
export class AppColorCode {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    moreColors: boolean = true;

    @State()
    private codeColorType: CodeColorType = undefined;

    @State()
    private codeColor: string;

    @State()
    private highlightLines: string;

    @State()
    private highlightColor: string;

    @Event() colorChange: EventEmitter<boolean>;

    async componentWillLoad() {
        this.codeColor = await this.initColor();
        await this.initCurrentHiglight();
    }

    private initCurrentHiglight(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.highlightLines = this.selectedElement && this.selectedElement.getAttribute('highlight-lines') ? this.selectedElement.getAttribute('highlight-lines') : null;
            this.highlightColor = this.selectedElement && this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-line-background') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-line-background') : '#3880ff';

            resolve();
        });
    }

    private selectColor($event: CustomEvent, colorFunction: Function): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement || !this.selectedElement.parentElement) {
                resolve();
                return;
            }

            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            colorFunction($event);

            this.emitColorChange();

            resolve();
        });
    }

    private setCodeColor = ($event: CustomEvent) => {
        this.codeColor = $event.detail.hex;
        this.selectedElement.style.setProperty(this.getStyle(), $event.detail.hex);
    };

    private setHighlightColor = ($event: CustomEvent) => {
        this.highlightColor = $event.detail.hex;
        this.selectedElement.style.setProperty('--deckgo-highlight-code-line-background', $event.detail.hex);
    };

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
        } else if (this.codeColorType === CodeColorType.LINE_NUMBERS) {
            return '--deckgo-highlight-code-line-numbers';
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
            } else if (this.codeColorType === CodeColorType.LINE_NUMBERS) {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-line-numbers') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-line-numbers') : '#999');
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

            this.emitColorChange();

            resolve();
        });
    }

    private async presentHighlightInfo() {
        const alert: HTMLIonAlertElement = await alertController.create({
            message: 'If you wish to highlight some specific lines of your code, list their line numbers separately using comma.<br/><br/>For example: 1,2 7,7 13,15<br/><br/>Which would highlight lines 1 to 2, line 7 and lines 13 to 15.',
            buttons: ['Ok']
        });

        return await alert.present();
    }

    private emitColorChange() {
        this.colorChange.emit(false);
    }

    render() {
        return [
            <ion-list>
                <ion-item-divider class="ion-padding-top">
                    <ion-label>Apply color to</ion-label>
                </ion-item-divider>

                <ion-item class="select">
                    <ion-label>Apply color to</ion-label>

                    <ion-select value={this.codeColorType} placeholder="Select a category"
                                onIonChange={(e: CustomEvent) => this.toggleColorType(e)}
                                class="ion-padding-start ion-padding-end">
                        <ion-select-option value={CodeColorType.COMMENTS}>Comments</ion-select-option>
                        <ion-select-option value={CodeColorType.FUNCTION}>Functions</ion-select-option>
                        <ion-select-option value={CodeColorType.KEYWORD}>Keywords</ion-select-option>
                        <ion-select-option value={CodeColorType.OPERATOR}>Operators</ion-select-option>
                        <ion-select-option value={CodeColorType.PUNCTUATION}>Punctuation</ion-select-option>
                        <ion-select-option value={CodeColorType.PROPERTY}>Properties</ion-select-option>
                        <ion-select-option value={CodeColorType.REGEX}>Regex</ion-select-option>
                        <ion-select-option value={CodeColorType.SELECTOR}>Selector</ion-select-option>
                        <ion-select-option value={CodeColorType.LINE_NUMBERS}>Line numbers</ion-select-option>
                    </ion-select>
                </ion-item>

                <ion-item disabled={this.codeColorType === undefined}>
                    <deckgo-color class="ion-padding-top ion-padding-bottom" onColorChange={($event: CustomEvent) => this.selectColor($event, this.setCodeColor)} color-hex={this.codeColor} more={this.moreColors}>
                        <ion-icon name="more" ios="md-mode" md="md-more" slot="more" aria-label="More" class="more"></ion-icon>
                    </deckgo-color>
                </ion-item>

                <ion-item-divider class="ion-padding-top">
                    <ion-label>Highlight lines</ion-label>
                    <button slot="end" class="info" onClick={() => this.presentHighlightInfo()}>
                        <ion-icon name="help"></ion-icon>
                    </button>
                </ion-item-divider>

                <ion-item class="with-padding">
                    <ion-input value={this.highlightLines} placeholder="List your lines here" debounce={500}
                               onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
                               onIonChange={() => this.highlightSelectedLines()}></ion-input>
                </ion-item>

                <ion-item disabled={!this.highlightLines}>
                    <deckgo-color class="ion-padding-top ion-padding-bottom" onColorChange={($event: CustomEvent) => this.selectColor($event, this.setHighlightColor)} color-hex={this.highlightColor} more={this.moreColors}>
                        <ion-icon name="more" ios="md-mode" md="md-more" slot="more" aria-label="More" class="more"></ion-icon>
                    </deckgo-color>
                </ion-item>
            </ion-list>
        ]
    }
}
