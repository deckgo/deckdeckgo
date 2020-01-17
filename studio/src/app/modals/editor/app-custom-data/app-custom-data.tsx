import {Component, Element, Listen, State, h} from '@stencil/core';

import {take} from 'rxjs/operators';

import {Deck} from '../../../models/data/deck';

import {StorageService} from '../../../services/storage/storage.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';

@Component({
    tag: 'app-custom-data',
    styleUrl: 'app-custom-data.scss'
})
export class AppCustomData {

    @Element() el: HTMLElement;

    private storageService: StorageService;
    private deckEditorService: DeckEditorService;

    @State()
    private files: StorageFile[];

    @State()
    private disableInfiniteScroll = false;

    private paginationNext: string | null;

    @State()
    private uploading: boolean = false;

    @State()
    private searchFolder: string;

    @State()
    private loading: boolean = true;

    constructor() {
        this.storageService = StorageService.getInstance();
        this.deckEditorService = DeckEditorService.getInstance();
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);

        this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
            await this.search(deck ? `data/${deck.id}` : 'data');
        });
    }

    @Listen('popstate', { target: 'window' })
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private selectData(storageFile: StorageFile): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!storageFile) {
                resolve();
                return;
            }

            if (this.uploading) {
                resolve();
                return;
            }

            await this.selectAndClose(storageFile);

            resolve();
        });
    }

    private selectAndClose(data: StorageFile): Promise<void> {
        return new Promise<void>(async (resolve) => {
            await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(data);

            resolve();
        });
    }

    private search(path: string): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const list: StorageFilesList = await this.storageService.getFiles(this.paginationNext, path);

            this.searchFolder = path;

            if (!list) {
                resolve();
                return;
            }

            if (!list.items || list.items.length <= 0) {
                this.emptyFiles();

                this.loading = false;

                resolve();
                return;
            }

            if (!this.files) {
                this.files = [];
            }
            
            this.files = [...this.files, ...list.items];

            this.paginationNext = list.nextPageToken;

            this.disableInfiniteScroll = list.items.length < this.storageService.maxQueryResults;

            this.loading = false;

            resolve();
        });
    }

    private emptyFiles() {
        this.files = [];

        this.disableInfiniteScroll = true;
    }

    private searchNext(e: CustomEvent<void>): Promise<void> {
        return new Promise<void>(async (resolve) => {
            await this.search(this.searchFolder);

            (e.target as HTMLIonInfiniteScrollElement).complete();

            resolve();
        });
    }

    private openFilePicker() {
        const filePicker: HTMLInputElement = this.el.querySelector('input');

        if (!filePicker) {
            return;
        }

        filePicker.click();
    }

    private upload(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const filePicker: HTMLInputElement = this.el.querySelector('input');

            if (!filePicker) {
                this.uploading = false;
                resolve();
                return;
            }

            if (filePicker.files && filePicker.files.length > 0) {
                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (deck && deck.data && deck.id) {
                        this.uploading = true;

                        const uploadInfo: StorageUploadInfo = {
                            maxSize: 10485760,
                            privateFile: true,
                            folder: `data/${deck.id}`,
                            folderMeta: {
                                deckName: deck.data.name
                            }
                        };

                        const storageFile: StorageFile = await this.storageService.uploadFile(filePicker.files[0], uploadInfo);

                        if (storageFile) {
                            await this.selectAndClose(storageFile);
                        }

                        this.uploading = false;
                    }
                });
            }

            resolve();
        });
    }

    private async switchFolder(folder: StorageFolder) {
        if (this.uploading) {
            return;
        }

        if (!folder.folder) {
            return;
        }

        this.loading = true;

        await this.emptyFiles();
        await this.search(`data/${folder.name}`);
    }

    private async switchRootFolder() {
        if (this.uploading) {
            return;
        }

        this.loading = true;

        await this.emptyFiles();
        await this.search(`data`);
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="tertiary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>

                    <ion-title class="ion-text-uppercase">Your data</ion-title>

                    {this.renderNavigateRoot()}
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                {this.renderData()}

                <input type="file" accept=".csv" onChange={() => this.upload()}/>

                <ion-infinite-scroll threshold="100px" disabled={this.disableInfiniteScroll}
                                     onIonInfinite={(e: CustomEvent<void>) => this.searchNext(e)}>
                    <ion-infinite-scroll-content
                        loadingText="Loading more images...">
                    </ion-infinite-scroll-content>
                </ion-infinite-scroll>
            </ion-content>,
            <ion-footer>
                <ion-toolbar>
                    <div class={this.uploading ? 'uploading' : undefined}>
                        {this.renderToolbarAction()}
                    </div>
                </ion-toolbar>
            </ion-footer>
        ];
    }

    private renderData() {
        if (!this.files || this.files.length <= 0) {
            return this.renderPlaceHolder();
        } else {
            return <div class="data-container">
                {this.renderFiles()}
            </div>;
        }
    }

    private renderFiles() {
        return (
            this.files.map((storageFile: StorageFile) => {
                if (storageFile.hasOwnProperty('folder') && (storageFile as StorageFolder).folder) {
                    return this.renderFolder(storageFile as StorageFolder);
                } else {
                    return this.renderFile(storageFile);
                }
            })
        );
    }

    private renderFolder(storageFile: StorageFolder) {
        return <div class="ion-padding data folder" custom-tappable onClick={() => this.switchFolder(storageFile)}>
            <ion-icon src="/assets/icons/ionicons/md-folder.svg"></ion-icon>
            <ion-label>{storageFile.displayName}</ion-label>
        </div>
    }

    private renderFile(storageFile: StorageFile) {
        return <div class="ion-padding data" custom-tappable onClick={() => this.selectData(storageFile)}>
            <ion-icon src="/assets/icons/file.svg"></ion-icon>
            <ion-label>{storageFile.name}</ion-label>
        </div>
    }

    private renderPlaceHolder() {
        if (this.loading) {
            return undefined;
        }

        return <div class="placeholder">
            <div>
                <ion-icon src="/assets/icons/file.svg"></ion-icon>
                <ion-label class="ion-text-center">Your collection of data is empty for this presentation</ion-label>
            </div>
        </div>
    }

    private renderToolbarAction() {
        if (!this.uploading) {
            return <ion-button onClick={() => this.openFilePicker()} shape="round" color="tertiary">
                <ion-icon name="cloud-upload" slot="start"></ion-icon>
                <ion-label>Upload a new data</ion-label>
            </ion-button>;
        } else {
            return [
                <ion-spinner color="tertiary"></ion-spinner>,
                <ion-label class="ion-padding-start">Upload in progress</ion-label>
            ];
        }
    }

    private renderNavigateRoot() {
        if (this.searchFolder === 'data') {
            return undefined;
        }

        return <ion-buttons slot="end" aria-label="Your presentations">
            <ion-button onClick={() => this.switchRootFolder()}>
                <ion-icon name="search"></ion-icon>
            </ion-button>
        </ion-buttons>;
    }

}
