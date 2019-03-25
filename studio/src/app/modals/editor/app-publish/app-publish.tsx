import {Component, Element, Listen, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

import {User} from '../../../models/user';

import {AuthService} from '../../../services/auth/auth.service';

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

    private authService: AuthService;

    @State()
    private user: User;

    @State()
    private today: string;

    constructor() {
        this.authService = AuthService.getInstance();
    }

    async componentWillLoad() {
        const options: DateTimeFormatOptions = { year: 'numeric', month: 'short', day: 'numeric' };
        this.today = new Intl.DateTimeFormat('en-US', options).format(new Date());
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);

        this.authService.watch().pipe(take(1)).subscribe(async (user: User) => {
            this.user = user;
        });
    }

    @Listen('window:popstate')
    async handleHardwareBackButton(_e: PopStateEvent) {
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
                <p>Edit the preview of your presentation and add or change tags (up to 5) to make your presentation more inviting to readers</p>

                <ion-card class="ion-no-margin">
                    <ion-card-content>
                        <div class="summary">

                            <ion-card-header>
                                <ion-card-title contenteditable class="ion-text-uppercase">Card Title</ion-card-title>

                                <ion-card-subtitle class="tags ion-text-lowercase">
                                    {this.renderTags()}
                                    {this.renderInputTags()}
                                </ion-card-subtitle>
                            </ion-card-header>

                            <p padding-start padding-end contenteditable class="content">Keep close to Nature's heart... and break clear away, once in
                                awhile,
                                and climb a mountain or spend a week in the woods. Wash your spirit clean.
                            </p>

                            <p class="author" padding>
                                <ion-label>{this.renderUser()} | {this.today}</ion-label>
                            </p>
                        </div>
                        <div class="preview">
                            <img src="./assets/img/deckdeckgo-logo.svg"/>
                        </div>
                    </ion-card-content>
                </ion-card>
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

    private renderUser() {
        if (this.user && this.user.name) {
            return <span>{this.user.name}</span>
        } else {
            return undefined;
        }
    }

}
