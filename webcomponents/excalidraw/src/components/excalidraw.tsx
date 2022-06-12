import {Component, Element, h, Host, Method, Prop, Watch} from '@stencil/core';
import {ExcalidrawScene, exportToBlob, getScene, renderExcalidraw, updateScene} from '../excalidraw/excalidraw.app';
import {jsonMapReplacer} from '../utils/json.utils';

@Component({
  tag: 'deckgo-excalidraw',
  styleUrl: 'excalidraw.scss',
  shadow: false
})
export class Excalidraw {
  @Element() host!: HTMLDeckgoExcalidrawElement;

  /**
   * An Excalidraw scene that contains app state and elements. On change, Excalidraw will be updated.
   */
  @Prop()
  scene: ExcalidrawScene;

  componentDidLoad() {
    renderExcalidraw({host: this.host, scene: this.scene});
  }

  /**
   * Export Excalidraw scene to blob - i.e. to image
   * @param type The mime type of the image. Default: image/webp
   */
  @Method()
  toBlob(type: string = 'image/webp'): Promise<Blob> {
    return exportToBlob(type);
  }

  /**
   * Export Excalidraw scene data to blob
   */
  @Method()
  async exportScene(): Promise<Blob | null> {
    const data: ExcalidrawScene = getScene();

    return new Blob([JSON.stringify(data, jsonMapReplacer)], {
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
