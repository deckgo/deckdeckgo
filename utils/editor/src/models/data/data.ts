export interface DataRecord<D> {
  id: string;
  data?: D;
  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}
