import type { CommandModule } from 'yargs'

const queryCommand: CommandModule = {
  command: 'query',
  describe: 'Run query command',
  handler: () => {
    console.log('🔍 Query command executed')
  }
}

export default queryCommand
