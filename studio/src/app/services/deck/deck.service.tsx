export class DeckService {

    private static instance: DeckService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!DeckService.instance) {
            DeckService.instance = new DeckService();
        }
        return DeckService.instance;
    }
}

