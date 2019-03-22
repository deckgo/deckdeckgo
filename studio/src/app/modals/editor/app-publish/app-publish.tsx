import {Component, Element, Listen, State} from '@stencil/core';

interface InputTargetEvent extends EventTarget {
    value: string;
}

@Component({
    tag: 'app-publish',
    styleUrl: 'app-publish.scss'
})
export class AppPublish {

    @Element() el: HTMLElement;

    @State()
    private tags: string[] = [];

    @State()
    private tagInput: string = null;

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private handleTagInput($event: UIEvent) {
        const tag: string = ($event.target as InputTargetEvent).value;

        if (tag && tag.trim().length > 0 && tag.charAt(tag.length - 1) === ' ' && this.tags && this.tags.indexOf(tag.trim()) === -1) {
            this.tags = [...this.tags, tag.trim()];
            this.tagInput = null;
        }
    }

    private removeTag(tag: string): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!tag) {
                resolve();
                return;
            }

            if (!this.tags) {
                resolve();
                return;
            }

            const index: number = this.tags.findIndex((actualTag: string) => {
                return tag === actualTag
            });

            if (index >= 0) {
                this.tags.splice(index, 1);
                this.tags = [...this.tags];
            }

            resolve();
        });
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Ready to publish?</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                <p>Add or change <strong>tags</strong> (up to 5) so readers know what your presentation is about</p>

                <div class="tags">
                    {this.renderTags()}
                    {this.renderInputTags()}
                </div>

                <p>Story Preview</p>

                <div>
                    Write a preview title
                    Write a preview subtitle
                    Include a high-quality image in your story to make it more inviting to readers.
                </div>
            </ion-content>
        ];
    }

    private renderTags() {
        if (!this.tags || this.tags.length <= 0) {
            return undefined;
        } else {
            return (
                this.tags.map((tag: string) => {
                    return (
                        <div class="chips">
                            <ion-icon name="close" custom-tappable onClick={() => this.removeTag(tag)}></ion-icon>
                            <ion-label>{tag}</ion-label>
                        </div>
                    )
                })
            );
        }
    }

    private renderInputTags() {
        if (this.tags && this.tags.length < 5) {
            return <input autofocus placeholder="Add a tag..." value={this.tagInput}
                          onInput={($event: UIEvent) => this.handleTagInput($event)}></input>
        } else {
            return undefined;
        }
    }

}
