import type { CommandModule } from 'yargs'
import { startVitest } from 'vitest/node'

const integrationCommand: CommandModule = {
  command: 'integration',
  describe: 'Run integration test suite',
  handler: async () => {
    console.log('🧪 Running integration tests...')

    await startVitest('test', ['--tags=lifecycle'], {
      include: ['tests/e2e/**/*.test.ts'],
      watch: false,
      reporters: ['default']
    })
  }
}

export default integrationCommand
