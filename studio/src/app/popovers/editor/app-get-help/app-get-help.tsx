import {Component, Element, h} from '@stencil/core';

@Component({
    tag: 'app-get-help',
    styleUrl: 'app-get-help.scss'
})
export class AppGetHelp {

    @Element() el: HTMLElement;

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    render() {
        return <div class="ion-padding">
            <h2>We are in development</h2>
            <p>If you need help or have any feedback, reach us on <a href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY" target="_blank">Slack</a>.</p>
            <p>Thank you in advance for your help 🙏</p>
            <div class="ion-text-center ion-padding-top"><ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>Got it</ion-button></div>
        </div>
    }

}
