import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-menu-footer',
  styleUrl: 'app-menu-footer.scss',
  shadow: true,
})
export class AppMenuFooter {
  render() {
    return (
      <footer>
        <p>Created with passion in Zürich🇨🇭</p>
      </footer>
    );
  }
}
