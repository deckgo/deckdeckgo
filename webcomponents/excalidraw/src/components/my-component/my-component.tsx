import { Component, h, Method, Element, Host, Prop, Watch } from '@stencil/core';
import { ExcalidrawScene, exportToBlob, getScene, renderExcalidraw, updateScene } from '../excalidraw';
import { replacer } from '../../json.utils';

@Component({
  tag: 'my-component',
  styleUrl: 'my-component.css',
  shadow: false
})
export class MyComponent {

  @Element() host!: HTMLMyComponentElement

  @Prop()
  scene: ExcalidrawScene;

  componentDidLoad() {
    renderExcalidraw({host: this.host, scene: this.scene});
  }

  @Method()
  toBlob(type: string = 'image/webp'): Promise<Blob> {
    return exportToBlob(type);
  }

  @Method()
  async exportScene(): Promise<Blob | null> {
    const data: ExcalidrawScene = getScene();

    return new Blob([JSON.stringify(data, replacer)], {
      type: 'application/json; charset=utf-8'
    });
  }

  @Watch('scene')
  onSceneChange() {
    updateScene(this.scene);
  }

  render() {
    return <Host></Host>;
  }
}
