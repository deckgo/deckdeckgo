import {Component} from '@stencil/core';


@Component({
    tag: 'app-avatar',
    styleUrl: 'app-avatar.scss',
    shadow: true
})
export class AppAvatar {

    render() {
        return <ion-avatar>
            <img src="https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg"/>
        </ion-avatar>
    }

}
