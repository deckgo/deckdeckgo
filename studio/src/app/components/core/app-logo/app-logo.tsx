import {Component, h} from '@stencil/core';
import {AppIcon} from '../app-icon/app-icon';

@Component({
  tag: 'app-logo',
  styleUrl: 'app-logo.scss',
  shadow: true
})
export class AppLogo {
  render() {
    return (
      <div>
        <AppIcon name="deckdeckgo-logo-round" path="img" ariaLabel="" ariaHidden={true}></AppIcon>
      </div>
    );
  }
}
