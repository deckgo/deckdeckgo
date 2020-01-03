import {Utils} from '../core/utils';

interface GoogleFont {
    id: string;
    name: string;
}

export class FontsUtils {

    private static fonts: GoogleFont[]  = [
        {
            id: 'google-fonts-lora',
            name: 'Lora'
        },
        {
            id: 'google-fonts-roboto',
            name: 'Roboto'
        },
        {
            id: 'google-fonts-open-sans',
            name: 'Open Sans'
        },
        {
            id: 'google-fonts-montserrat',
            name: 'Montserrat'
        },
        {
            id: 'google-fonts-cabin',
            name: 'Cabin'
        },
        {
            id: 'google-fonts-lato',
            name: 'Lato'
        },
        {
            id: 'google-fonts-muli',
            name: 'Muli'
        },
        {
            id: 'google-fonts-source-sans-pro',
            name: 'Source Sans Pro'
        },
        {
            id: 'google-fonts-libre-baskerville',
            name: 'Libre Baskerville'
        }
    ];

    static async loadAllGoogleFonts(googleFontsUrl: string) {
        try {
            const promises = this.fonts.map((font: GoogleFont) => {
                return Utils.injectCSS(font.id, googleFontsUrl + font.name.replace(' ', '+'));
            });

            await Promise.all(promises);
        } catch (err) {
            // We ignore this error. Show must go on aka will fallback on default font
        }
    }
}
