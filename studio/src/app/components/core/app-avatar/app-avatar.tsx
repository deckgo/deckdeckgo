import {Component, Prop} from '@stencil/core';


@Component({
    tag: 'app-avatar',
    styleUrl: 'app-avatar.scss',
    shadow: true
})
export class AppAvatar {

    @Prop() src: string;

    render() {
        if (this.src) {

            console.log(this.src);

            return <ion-avatar>
                <img src={this.src}/>
            </ion-avatar>
        } else {
            return <ion-avatar>
                <ion-icon md="md-person" ios="md-person"></ion-icon>
            </ion-avatar>
        }
    }

}
