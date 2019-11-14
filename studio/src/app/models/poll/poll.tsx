export interface PollDataAnswer {
    key: number | string;
    title: string;
    value: number;
}

export interface PollData {
    label: string;
    values: PollDataAnswer[];
}

export interface Poll {
    key: string;
    poll: PollData;
}
