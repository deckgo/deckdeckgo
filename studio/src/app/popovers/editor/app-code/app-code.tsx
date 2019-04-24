import {Component, Element, Event, EventEmitter, Prop} from '@stencil/core';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

@Component({
    tag: 'app-code',
    styleUrl: 'app-code.scss'
})
export class AppCode {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Event() private slideDidChange: EventEmitter<HTMLElement>;

    private hidePopoverTimer: number;

    private prismService: PrismService;

    private currentLanguage: string = 'javascript';
    private languages: PrismLanguage[];

    private commentsColor: string;

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

    async closePopover(e: CustomEvent) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            language: e && e.detail && e.detail.value ? e.detail.value : null
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

            this.commentsColor = $event.target.value;

            this.selectedElement.style.setProperty('--deckgo-highlight-code-token-comment', $event.target.value);

            this.slideDidChange.emit(this.selectedElement.parentElement);

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

    render() {
        return <ion-list>
            <ion-item>
                <ion-label>Language</ion-label>
                <ion-select value={this.currentLanguage} onIonChange={(e: CustomEvent) => this.closePopover(e)}>
                    {this.renderSelectOptions()}
                </ion-select>
            </ion-item>

            <ion-item>
                <ion-label>Comments</ion-label>
                <input type="color" name="color-picker" value={this.commentsColor} onChange={(e) => this.selectColor(e)}></input>
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
