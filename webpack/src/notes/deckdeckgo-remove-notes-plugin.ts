const resourcesRemoveNotes = require('./deckdeckgo-remove-notes-resources');

const fsNotes = require('fs');

const jsdom = require('jsdom');
const {JSDOM} = jsdom;

const writeFileNotes = require('util').promisify(fsNotes.writeFile);

class DeckDeckGoRemoveNotesPlugin {

    constructor() {
    }

    apply(compiler) {
        compiler.hooks.done.tap('DeckDeckGoRemoveNotesPlugin', async () => {
            const indexFile: boolean = fsNotes.existsSync(resourcesRemoveNotes.Constants.SRC);
            if (indexFile) {
                JSDOM.fromFile(resourcesRemoveNotes.Constants.SRC).then(dom => {
                    const notesSlots = dom.window.document.querySelectorAll('[slot="notes"]');
                    if (notesSlots && notesSlots.length > 0) {
                        this.removeNotes(notesSlots);
                    }

                    this.removeButtonSlideNotes(dom);

                    writeFileNotes(resourcesRemoveNotes.Constants.SRC, dom.serialize(), 'utf8');
                });
            }
        });
    }

    private removeButtonSlideNotes(dom) {
        const button = dom.window.document.querySelector('#buttonSlideNotes');

        if (button && button.parentNode) {
            button.parentNode.removeChild(button);
        }
    }

    private removeNotes(notesSlots) {
        for (let i = 0; i < notesSlots.length; i++) {
            const slot = notesSlots[i];

            if (slot.parentNode) {
                slot.parentNode.removeChild(slot);
            }
        }
    }
}

module.exports = DeckDeckGoRemoveNotesPlugin;
