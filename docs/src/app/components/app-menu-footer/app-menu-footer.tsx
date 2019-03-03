import {Component} from '@stencil/core';

@Component({
  tag: 'app-menu-footer',
  styleUrl: 'app-menu-footer.scss',
  shadow: true
})
export class AppMenuFooter {

  render() {
    return <footer>
      <p>
        Created by <a href="https://twitter.com/daviddalbusco">David Dal Busco</a> | Zürich🇨🇭
      </p>
    </footer>
  }
}
