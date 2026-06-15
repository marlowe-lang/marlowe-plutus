import { Client } from "pg";

export interface DbConfig {
  host: string;
  port: number;
  database: string;
  user?: string;
  password?: string;
}

export async function connectDB(config: DbConfig): Promise<Client> {
  const client = new Client(config);
  await client.connect();
  return client;
}

export interface CreateTxOutRecord {
  txid: Buffer;
  txix: number;
  blockid: Buffer;
  metadata: Buffer | null;
  slotno: bigint;
  blockno: bigint;
}

export interface ApplyTx {
  txid: Buffer;
  createtxid: Buffer;
  createtxix: number;
  blockid: Buffer;
  invalidbefore: Date;
  invalidhereafter: Date;
  metadata: Buffer | null;
  inputtxid: Buffer;
  inputtxix: number;
  inputs: Buffer;
  outputtxix: number | null;
  slotno: bigint;
  blockno: bigint;
}

export async function queryCreateTxOut(
  client: Client
): Promise<CreateTxOutRecord[]> {
  const result = await client.query<CreateTxOutRecord>(
    "SELECT txid, txix, blockid, metadata, slotno, blockno FROM marlowe.createtxout"
  );
  return result.rows;
}

export async function queryApplyTx(
  client: Client
): Promise<ApplyTx[]> {
  const result = await client.query<ApplyTx>(
    "SELECT txid, createtxid, createtxix, blockid, invalidbefore, invalidhereafter, metadata, inputtxid, inputtxix, inputs, outputtxix, slotno, blockno FROM marlowe.applytx"
  );
  return result.rows;
}

export async function queryTxOut(client: Client) {
  const result = await client.query(
    "SELECT txid, txix, blockid, address, lovelace FROM marlowe.txout"
  );
  return result.rows;
}

export function txIdToHex(buffer: Buffer): string {
  return buffer.toString("hex");
}

export function hexToTxId(hex: string): Buffer {
  return Buffer.from(hex, "hex");
}
