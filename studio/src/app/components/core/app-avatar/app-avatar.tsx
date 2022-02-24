import {Component, h, Prop} from '@stencil/core';
import {AppIcon} from '../app-icon/app-icon';

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
          <AppIcon name="person" ariaLabel={this.ariaLabel}></AppIcon>
        </ion-avatar>
      );
    }
  }
}
