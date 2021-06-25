import {Component, h, Prop, Event, EventEmitter} from '@stencil/core';

import {Expanded} from '../../../../types/core/settings';

@Component({
  tag: 'app-expansion-panel',
  styleUrl: 'app-expansion-panel.scss'
})
export class AppExpansionPanel {
  container!: HTMLDivElement;

  @Prop()
  expander: boolean = true;

  @Prop({mutable: true})
  expanded: Expanded = 'open';

  @Event()
  expansion: EventEmitter<Expanded>;

  // Source animation: https://css-tricks.com/using-css-transitions-auto-dimensions/

  componentDidLoad() {
    if (this.expanded === 'close') {
      this.container.style.height = 0 + 'px';
    }
  }

  private toggle($event: UIEvent) {
    $event.stopPropagation();

    if (this.expanded === 'close') {
      this.expand();
    } else {
      this.collapse();
    }

    this.expansion.emit(this.expanded);
  }

  private collapse() {
    if (!this.container) {
      return;
    }

    const sectionHeight: number = this.container.scrollHeight;
    const elementTransition = this.container.style.transition;
    this.container.style.transition = '';

    requestAnimationFrame(() => {
      this.container.style.height = sectionHeight + 'px';
      this.container.style.transition = elementTransition;

      requestAnimationFrame(() => {
        this.container.style.height = 0 + 'px';
      });
    });

    this.expanded = 'close';
  }

  private expand() {
    if (!this.container) {
      return;
    }

    const sectionHeight: number = this.container.scrollHeight;
    this.container.style.height = sectionHeight + 'px';

    this.container.addEventListener(
      'transitionend',
      () => {
        this.container.style.height = '';
      },
      {once: true}
    );

    this.expanded = 'open';
  }

  render() {
    return (
      <article class={this.expanded}>
        <ion-item onClick={($event: UIEvent) => this.toggle($event)} class={this.expander ? undefined : 'hidden'}>
          <div>
            <slot name="title"></slot>
          </div>
          <div slot="start">
            <slot name="icon">
              <ion-icon src="/assets/icons/ionicons/chevron-down.svg"></ion-icon>
            </slot>
          </div>
          <div slot="end">
            <slot name="info"></slot>
          </div>
        </ion-item>

        <div ref={(el) => (this.container = el as HTMLDivElement)}>
          <slot></slot>
        </div>
      </article>
    );
  }
}
