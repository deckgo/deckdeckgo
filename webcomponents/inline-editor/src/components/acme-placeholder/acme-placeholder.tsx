import { Component, h } from '@stencil/core';

@Component({
  tag: 'acme-placeholder',
  shadow: true
})
export class AcmePlaceholder {
  render() {
    return <slot />;
  }
}
