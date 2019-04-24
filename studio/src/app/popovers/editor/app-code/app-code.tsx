import {Component, Element, EventEmitter, Prop} from '@stencil/core';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

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

    private commentsColor: string;
    private punctuationColor: string;

    constructor() {
        this.prismService = PrismService.getInstance();
    }

    async componentWillLoad() {
        this.languages = await this.prismService.getLanguages();

        await this.initCurrentLanguage();
    }

    private initCurrentLanguage(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.currentLanguage = this.selectedElement && this.selectedElement.getAttribute('language') ? this.selectedElement.getAttribute('language') : 'javascript';

            resolve();
        });
    }

    private selectColor($event, updateColor: Function): Promise<void> {
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

            updateColor($event.target.value);

            this.emitCodeDidChange();

            resolve();
        });
    }

    private updateCommentsColor = (color: string) => {
        this.commentsColor = color;
        this.selectedElement.style.setProperty('--deckgo-highlight-code-token-comment', color);
    };

    private updatePunctuationColor = (color: string) => {
        this.punctuationColor = color;
        this.selectedElement.style.setProperty('--deckgo-highlight-code-token-punctuation', color);
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

    render() {
        return <ion-list>
            <ion-item>
                <ion-label>Language</ion-label>
                <ion-select value={this.currentLanguage} onIonChange={(e: CustomEvent) => this.toggleCodeLanguage(e)}>
                    {this.renderSelectOptions()}
                </ion-select>
            </ion-item>

            <ion-item>
                <ion-label>Comments</ion-label>
                <input type="color" name="color-picker" value={this.commentsColor}
                       onChange={(e) => this.selectColor(e, this.updateCommentsColor)}></input>
            </ion-item>

            <ion-item>
                <ion-label>Punctuation</ion-label>
                <input type="color" name="color-picker" value={this.punctuationColor}
                       onChange={(e) => this.selectColor(e, this.updatePunctuationColor)}></input>
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
