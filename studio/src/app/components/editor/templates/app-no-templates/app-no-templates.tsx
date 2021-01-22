import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-no-templates',
  styleUrl: 'app-no-templates.scss',
})
export class AppNoTemplates {
  render() {
    return (
      <ion-label>
        You do not have any personal templates yet. Follow this{' '}
        <a href="https://github.com/deckgo/template-kit" rel="noopener norefferer" target="_blank" class="tutorial">
          guide
        </a>{' '}
        to get started.
      </ion-label>
    );
  }
}
