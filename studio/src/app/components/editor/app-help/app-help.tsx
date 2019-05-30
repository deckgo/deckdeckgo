import {Component, State} from '@stencil/core';

import {get, set} from 'idb-keyval';

@Component({
    tag: 'app-help',
    styleUrl: 'app-help.scss'
})
export class AppHelp {

    @State()
    private helpDisplayedOnce: boolean = true;

    async componentWillLoad() {
        this.helpDisplayedOnce = await get<boolean>('deckdeckgo_display_help');
    }

    private async close() {
        this.helpDisplayedOnce = true;
        await set('deckdeckgo_display_help', this.helpDisplayedOnce);
    }

    render() {
        if (!this.helpDisplayedOnce) {
            return <aside>
                <div>
                    <p>We are in development, we need your help. Send us feedback on <a href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNjMyNTk2NTQwODk5LTAxZjAwZWQwODQyZDg1ZDA5ODhlOTE3OGMwZjhmYjY3NDRhZjViZTRiNWU3OGU3MjYyNjE1OWE3NzNkZmQ3ZWI" target="_blank">Slack</a>. You are awesome!</p>
                    <button onClick={() => this.close()}><ion-icon name="close"></ion-icon></button>
                </div>
            </aside>
        } else {
            return undefined;
        }
    }
}
