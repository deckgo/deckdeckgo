import { Component, h } from '@stencil/core';

@Component({
  tag: 'acme-placeholder',
  // styleUrl: 'acme-placeholder.scss',
  shadow: true
})
export class AcmePlaceholder {
  render() {
    return <slot />;
  }
}
