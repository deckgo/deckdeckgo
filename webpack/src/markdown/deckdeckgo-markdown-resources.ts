class DeckDeckGoMarkdownResources {

    static get Constants(): any {
        return {
            SRC: './src/index.md',
            OUTPUT: './dist/index.html',
            REGEX: {
                SLIDE: {
                    SLIDE: /---\s?((title|content|split|gif|youtube|code|author|chart|qrcode)[\s\S]*)/i,
                    SLIDE_WITH_SLOT_CONTENT: /(title|content|qrcode)/i,
                    SLIDE_WITH_TWO_SLOTS: /(split|gif)/i,
                    SLIDE_SPLIT: /(split)/i
                },
                COMPONENT: {
                    CODE: /^(```)([\s\S]*)/i
                },
                SLOT: {
                    TITLE: /^(#{1,6}\s*[\S]+)/gi,
                    SLOT: /^(.*?)[>]/gm,
                    NOTES: /^(\*\*\*)(\sshow)?/si
                },
                DECK: /(<deckgo-deck(.+?)>)(.*?)(<\/deckgo-deck>)/si,
                IONIC_DECKDECKGO_HTML_PARSER: /^(<ion-|<deckgo-)/
            },
            TAG: {
                SLIDE: 'deckgo-slide-',
                SLOT: {
                    TITLE: '$1 slot="title">',
                    CONTENT: '<div slot="content">',
                    SPLIT_START: '<div slot="start">',
                    SPLIT_END: '<div slot="end">',
                    GIF_HEADER: '<div slot="header">',
                    GIF_FOOTER: '<div slot="footer">',
                    NOTES: '<div slot="notes">',
                    NOTES_SHOW: '<div slot="notes" show>',
                    CODE_BEGIN: '<code slot="code">',
                    CODE_END: '</code>',
                    END: '</div>'
                },
                COMPONENT: {
                    CODE: 'deckgo-highlight-code'
                },
                DECK_END: '</deckgo-deck>'
            }
        };
    }

}

module.exports = DeckDeckGoMarkdownResources;
