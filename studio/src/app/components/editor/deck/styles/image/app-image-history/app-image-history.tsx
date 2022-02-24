import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';
import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';
import {ImageHistoryService} from '../../../../../../services/editor/image-history/image-history.service';
import i18n from '../../../../../../stores/i18n.store';

@Component({
  tag: 'app-image-history',
  styleUrl: 'app-image-history.scss'
})
export class AppImageHistory {
  @Prop()
  deck: boolean = false;

  @Prop()
  slide: boolean = false;

  private imageHistoryService: ImageHistoryService;

  @State()
  private imagesHistory: (UnsplashPhoto | TenorGif | StorageFile | Waves)[];

  @Event()
  selectImage: EventEmitter<UnsplashPhoto | TenorGif | StorageFile | Waves>;

  constructor() {
    this.imageHistoryService = ImageHistoryService.getInstance();
  }

  async componentDidLoad() {
    await this.initImagesHistory();
  }

  private async initImagesHistory() {
    let imagesHistory: (UnsplashPhoto | TenorGif | StorageFile | Waves)[] = await this.imageHistoryService.get();

    if (!imagesHistory || imagesHistory.length <= 0) {
      return;
    }

    // Filter waves for image elements
    if (!this.deck && !this.slide) {
      imagesHistory = [...imagesHistory.filter((img) => !img.hasOwnProperty('viewBox'))];
    }

    this.imagesHistory = [...imagesHistory];
  }

  render() {
    return (
      <ion-list>
        <ion-item-divider class="ion-padding-top ion-margin-top">
          <ion-label>{i18n.state.editor.history}</ion-label>
        </ion-item-divider>

        {this.renderImagesHistory()}
      </ion-list>
    );
  }

  private renderImagesHistory() {
    if (this.imagesHistory?.length > 0) {
      return this.renderImages();
    }

    return (
      <ion-item class="history-empty">
        <ion-label class="ion-text-wrap">
          <small>{i18n.state.editor.no_images}</small>
        </ion-label>
      </ion-item>
    );
  }

  private renderImages() {
    return (
      <div class="history-photos ion-padding">
        {this.imagesHistory.map((history: UnsplashPhoto | TenorGif | StorageFile | Waves, index: number) => (
          <article custom-tappable key={`history-${index}`} onClick={() => this.selectImage.emit(history)}>
            <app-asset-image image={history}></app-asset-image>
          </article>
        ))}
      </div>
    );
  }
}
