import type { CommandModule } from 'yargs'
import { connectDB, queryCreateTxOut, queryApplyTx, txIdToHex } from './marloweIndexer/db.js'

const queryCommand: CommandModule = {
  command: 'query',
  describe: 'Query marlowe DB tables (createtxout and applytx)',
  handler: async () => {
    const client = await connectDB({
      host: '127.0.0.1',
      port: 15432,
      database: 'marlowe'
    })

    console.log('🔍 Querying createtxout table...')
    const createTxOuts = await queryCreateTxOut(client)
    console.log(`Found ${createTxOuts.length} create txout(s):`)
    for (const row of createTxOuts) {
      console.log(`  - txid: ${txIdToHex(row.txid)}, txix: ${row.txix}, slotno: ${row.slotno}, blockno: ${row.blockno}`)
    }

    console.log('\n🔍 Querying applytx table...')
    const applyTxs = await queryApplyTx(client)
    console.log(`Found ${applyTxs.length} apply tx(s):`)
    for (const row of applyTxs) {
      console.log(`  - txid: ${txIdToHex(row.txid)}, createtxid: ${txIdToHex(row.createtxid)}, slotno: ${row.slotno}`)
    }

    await client.end()
    console.log('\n✅ Query complete')
  }
}

export default queryCommand
