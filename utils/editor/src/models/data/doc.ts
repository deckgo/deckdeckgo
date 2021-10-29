export interface DocData {
  name: string;

  owner_id: string | undefined;

  paragraphs?: string[];

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export interface Doc {
  id: string;
  data: DocData;
}
