import {Component, Prop, h} from '@stencil/core';

@Component({
  tag: 'app-avatar',
  styleUrl: 'app-avatar.scss',
  shadow: true
})
export class AppAvatar {
  @Prop() src: string;
  @Prop() ariaLabel: string;

  render() {
    if (this.src) {
      return (
        <ion-avatar>
          <img src={this.src} alt={this.ariaLabel} />
        </ion-avatar>
      );
    } else {
      return (
        <ion-avatar>
          <ion-icon src="/assets/icons/ionicons/person.svg" aria-label={this.ariaLabel}></ion-icon>
        </ion-avatar>
      );
    }
  }
}
