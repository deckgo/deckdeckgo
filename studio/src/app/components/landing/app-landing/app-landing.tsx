import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-landing',
    styleUrl: 'app-landing.scss',
    shadow: false
})
export class AppLanding {

    render() {
        return <h1>Hello World</h1>
    }

}
