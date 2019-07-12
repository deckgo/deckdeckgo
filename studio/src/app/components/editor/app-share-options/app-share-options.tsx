import {Component, Event, EventEmitter, h} from '@stencil/core';

import {MoreAction} from '../../../utils/editor/more-action';

@Component({
    tag: 'app-share-options',
    styleUrl: 'app-share-options.scss',
    shadow: true
})
export class AppMoreShareOptions {

    @Event() selectedOption: EventEmitter<MoreAction>;

    render() {
        return [
            <a onClick={() => this.selectedOption.emit(MoreAction.PUBLISH)}><p>Update published presentation</p></a>,
            <a onClick={() => this.selectedOption.emit(MoreAction.SHARE)}><p>Share</p></a>
        ]
    }

}
