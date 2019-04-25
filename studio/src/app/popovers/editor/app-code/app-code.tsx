import {Component, Element, EventEmitter, Prop, State} from '@stencil/core';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

enum CodeColorType {
    COMMENTS,
    PUNCTUATION
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

    private hidePopoverTimer: number;

    private prismService: PrismService;

    private currentLanguage: string = 'javascript';
    private languages: PrismLanguage[];

    private codeColorType: CodeColorType = CodeColorType.COMMENTS;

    @State()
    private codeColor: string;

    constructor() {
        this.prismService = PrismService.getInstance();
    }

    async componentWillLoad() {
        this.languages = await this.prismService.getLanguages();

        await this.initCurrentLanguage();
    }

    private initCurrentLanguage(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.currentLanguage = this.selectedElement && this.selectedElement.getAttribute('language') ? this.selectedElement.getAttribute('language') : 'javascript';
            this.codeColor = await this.initColor();

            resolve();
        });
    }

    private selectColor($event): Promise<void> {
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

            this.codeColor = $event.target.value;
            this.selectedElement.style.setProperty(this.getStyle(), $event.target.value);

            this.emitCodeDidChange();

            resolve();
        });
    }

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
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-punctuation') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-punctuation') : null);
            } else {
                resolve(this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-comment') ? this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-comment') : null);
            }
        });
    }

    render() {
        return <ion-list>
            <ion-item-divider><ion-label>Language</ion-label></ion-item-divider>

            <ion-select value={this.currentLanguage} onIonChange={(e: CustomEvent) => this.toggleCodeLanguage(e)} class="ion-padding-start ion-padding-end">
                {this.renderSelectOptions()}
            </ion-select>

            <ion-item-divider class="ion-padding-top"><ion-label>Colors</ion-label></ion-item-divider>

            <ion-select value={this.codeColorType} onIonChange={(e: CustomEvent) => this.toggleColorType(e)} class="ion-padding-start ion-padding-end">
                <ion-select-option value={CodeColorType.COMMENTS}>Comments</ion-select-option>
                <ion-select-option value={CodeColorType.PUNCTUATION}>Punctuation</ion-select-option>
            </ion-select>

            <ion-item>
                <ion-label>Value</ion-label>
                <input type="color" value={this.codeColor} onChange={(e) => this.selectColor(e)}></input>
            </ion-item>
        </ion-list>
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
