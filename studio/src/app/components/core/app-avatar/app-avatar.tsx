import {Component, Prop} from '@stencil/core';


@Component({
    tag: 'app-avatar',
    styleUrl: 'app-avatar.scss',
    shadow: true
})
export class AppAvatar {

    @Prop() src: string;

    render() {
        return <ion-avatar>
            <img src={this.src}/>
        </ion-avatar>
    }

}
