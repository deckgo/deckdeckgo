import {Component, Element, Prop} from '@stencil/core';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

@Component({
    tag: 'app-code',
    styleUrl: 'app-code.scss'
})
export class AppCode {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    private prismService: PrismService;

    private currentLanguage: string = 'javascript';
    private languages: PrismLanguage[];

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
            language: e && e.detail &&e.detail.value ? e.detail.value : null
        });
    }

    render() {
        return <ion-item>
            <ion-label>Language</ion-label>
            <ion-select value={this.currentLanguage} onIonChange={(e: CustomEvent) => this.closePopover(e)}>
                {this.renderSelectOptions()}
            </ion-select>
        </ion-item>
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
