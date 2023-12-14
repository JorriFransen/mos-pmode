
local function async_command(command)
    vim.cmd.let('g:asyncrun_open = 14')
    vim.cmd.wa()
    vim.cmd.AsyncRun(command)
    vim.cmd.redraw()
end

function Compile()
    vim.cmd.wa()

    -- async_command('make --no-print-directory')
    async_command('make')
end

function Clean()
    vim.cmd.wa()

    async_command('make --no-print-directory clean')
end

function Run()
    async_command('make run')
end

function RunTests()
    async_command('make debug')
end
