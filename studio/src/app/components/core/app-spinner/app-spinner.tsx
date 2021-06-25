import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-spinner',
  styleUrl: 'app-spinner.scss'
})
export class AppSpinner {
  render() {
    return (
      <div class="spinner">
        <ion-spinner color="medium"></ion-spinner>
      </div>
    );
  }
}
