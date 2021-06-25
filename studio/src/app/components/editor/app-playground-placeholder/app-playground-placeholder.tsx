import {Component, h, Host} from '@stencil/core';

@Component({
  tag: 'app-playground-placeholder',
  styleUrl: 'app-playground-placeholder.scss',
  shadow: true
})
export class AppPlaygroundPlaceholder {
  render() {
    return (
      <Host>
        <div class="header"></div>
        <div class="column">
          <div>HTML</div>
          <div>1</div>
        </div>
        <div class="column">
          <div>CSS</div>
          <div>1</div>
        </div>
        <div class="column">
          <div>JS</div>
          <div>1</div>
        </div>
        <div class="row"></div>
        <div class="footer"></div>
      </Host>
    );
  }
}
