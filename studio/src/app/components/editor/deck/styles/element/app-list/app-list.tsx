import {SlotType, SlotUtils} from '@deckdeckgo/studio';
import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';
import i18n from '../../../../../../stores/i18n.store';
import settingsStore from '../../../../../../stores/settings.store';
import undoRedoStore from '../../../../../../stores/undo-redo.store';
import {EditMode, Expanded} from '../../../../../../types/core/settings';
import {ListStyle} from '../../../../../../types/editor/list-style';
import {SettingsUtils} from '../../../../../../utils/core/settings.utils';
import {ListUtils} from '../../../../../../utils/editor/list.utils';
import {setStyle} from '../../../../../../utils/editor/undo-redo.deck.utils';

@Component({
  tag: 'app-list',
  styleUrl: 'app-list.scss'
})
export class AppList {
  @Prop()
  selectedTarget: HTMLElement;

  @State()
  private listType: SlotType.OL | SlotType.UL | undefined;

  @State()
  private selectedStyle: ListStyle | undefined;

  @Event() toggleList: EventEmitter<SlotType.OL | SlotType.UL>;

  @Event() listStyleChanged: EventEmitter<ListStyle>;

  @State()
  private listStyleCSS: string;

  private destroyListener;

  private ignoreUpdateStyle: boolean = false;

  async componentWillLoad() {
    this.listType = ListUtils.isElementList(this.selectedTarget);

    await this.initListStyle();
    await this.initListStyleCSS();

    this.destroyListener = settingsStore.onChange('editMode', async (edit: EditMode) => {
      if (edit === 'css') {
        await this.initListStyleCSS();
        return;
      }

      await this.initListStyle();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async initListStyle() {
    this.selectedStyle = ListUtils.getListElementType(this.selectedTarget);
  }

  private async initListStyleCSS() {
    if (SlotUtils.isNodeRevealList(this.selectedTarget)) {
      this.listStyleCSS = this.selectedTarget.style['--reveal-list-style'];
    } else {
      this.listStyleCSS = this.selectedTarget.style.listStyleType;
    }
  }

  private async setListType($event: CustomEvent) {
    if (!this.selectedTarget || !$event || !$event.detail) {
      return;
    }

    this.listType = $event.detail.value;

    // Remove style with undo redo as we are going to replace the element in the dom
    if (SlotUtils.isNodeRevealList(this.selectedTarget)) {
      this.selectedTarget.style['--reveal-list-style'] = '';
    } else {
      this.selectedTarget.style.listStyleType = '';
    }

    // We have to clear the history undo redo too
    undoRedoStore.reset();

    this.toggleList.emit(this.listType);
  }

  private async setListStyle($event: CustomEvent) {
    if (!this.selectedTarget || !$event || !$event.detail) {
      return;
    }

    await this.applyStyle($event.detail.value);
  }

  private async applyStyle(style: ListStyle) {
    this.selectedStyle = style;

    this.updateStyle({
      property: SlotUtils.isNodeRevealList(this.selectedTarget) ? '--reveal-list-style' : 'list-style-type',
      value: this.selectedStyle
    });

    this.listStyleChanged.emit();
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.listStyleCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateLetterSpacingCSS() {
    if (SlotUtils.isNodeRevealList(this.selectedTarget)) {
      this.selectedTarget.style['--reveal-list-style'] = this.listStyleCSS;
    } else {
      this.selectedTarget.style.listStyleType = this.listStyleCSS;
    }

    this.listStyleChanged.emit();

    this.updateStyle({
      property: SlotUtils.isNodeRevealList(this.selectedTarget) ? '--reveal-list-style' : 'list-style-type',
      value: this.listStyleCSS
    });

    this.listStyleChanged.emit();
  }

  private updateStyle(property: {property: string; value: string | null}) {
    if (this.ignoreUpdateStyle) {
      this.ignoreUpdateStyle = false;
      return;
    }

    setStyle(this.selectedTarget, {
      properties: [property],
      type: 'element',
      updateUI: async (_value: string) => {
        // ion-change triggers the event each time its value changes, because we re-render, it triggers it again
        this.ignoreUpdateStyle = true;

        if (settingsStore.state.editMode === 'css') {
          await this.initListStyleCSS();
          return;
        }

        await this.initListStyle();
      }
    });
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.list}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({list: $event.detail})}
      >
        <ion-label slot="title">{i18n.state.editor.list}</ion-label>

        <ion-list>
          <ion-item class="select">
            <ion-label>{i18n.state.editor.list}</ion-label>

            <ion-select
              value={this.listType}
              placeholder={i18n.state.editor.list}
              onIonChange={($event: CustomEvent) => this.setListType($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end"
            >
              <ion-select-option value={SlotType.OL}>{i18n.state.editor.ordered}</ion-select-option>
              <ion-select-option value={SlotType.UL}>{i18n.state.editor.unordered}</ion-select-option>
            </ion-select>
          </ion-item>
        </ion-list>

        <ion-list>
          <ion-item class="select properties">
            <ion-label>{i18n.state.editor.list_style}</ion-label>
            <ion-select
              value={this.selectedStyle}
              placeholder={i18n.state.editor.list_style}
              onIonChange={($event: CustomEvent) => this.setListStyle($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end"
            >
              {this.listType === SlotType.OL ? this.renderOrderedStyles() : this.renderUnorderedStyles()}
            </ion-select>
          </ion-item>

          <ion-item class="with-padding css">
            <ion-input
              value={this.listStyleCSS}
              placeholder="list-style-type"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={() => this.updateLetterSpacingCSS()}
            ></ion-input>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderOrderedStyles() {
    return [
      <ion-select-option value={ListStyle.DECIMAL}>{i18n.state.editor.decimal}</ion-select-option>,
      <ion-select-option value={ListStyle.DECIMAL_LEADING}>{i18n.state.editor.decimal_with_zero}</ion-select-option>,
      <ion-select-option value={ListStyle.LATIN_LOWER}>{i18n.state.editor.latin_lowercase}</ion-select-option>,
      <ion-select-option value={ListStyle.LATIN_UPPER}>{i18n.state.editor.latin_uppercase}</ion-select-option>,
      <ion-select-option value={ListStyle.ROMAN_LOWER}>{i18n.state.editor.roman_lowercase}</ion-select-option>,
      <ion-select-option value={ListStyle.ROMAN_UPPER}>{i18n.state.editor.roman_uppercase}</ion-select-option>
    ];
  }

  private renderUnorderedStyles() {
    return [
      <ion-select-option value={ListStyle.BULLET}>{i18n.state.editor.bullet}</ion-select-option>,
      <ion-select-option value={ListStyle.CIRCLE}>{i18n.state.editor.circle}</ion-select-option>,
      <ion-select-option value={ListStyle.SQUARE}>{i18n.state.editor.square}</ion-select-option>
    ];
  }
}
