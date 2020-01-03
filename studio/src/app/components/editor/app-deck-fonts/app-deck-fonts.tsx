import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';

import {FontsUtils} from '../../../utils/editor/fonts.utils';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {EnvironmentGoogleConfig} from '../../../services/core/environment/environment-config';

@Component({
    tag: 'app-deck-fonts',
    styleUrl: 'app-deck-fonts.scss'
})
export class AppDeckTransition {

    @Prop()
    deckElement: HTMLElement;

    @Event() fontsChange: EventEmitter<void>;

    async componentWillLoad() {
        const google: EnvironmentGoogleConfig = EnvironmentConfigService.getInstance().get('google');
        await FontsUtils.loadAllGoogleFonts(google.fontsUrl);
    }

    render() {
        return <div class="container ion-margin-top ion-margin-bottom">
            Fonts TODO
        </div>
    }
}
