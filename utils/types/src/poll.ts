export interface DeckdeckgoPollAnswer {
    key: number | string;
    label: string;
}

export interface DeckdeckgoPollQuestion {
    label: string;
    values: DeckdeckgoPollAnswer[];
    answered: boolean;
}

export interface DeckdeckgoPoll {
    key: string;
    poll: DeckdeckgoPollQuestion;
}
