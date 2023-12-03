var lines = File.ReadAllLines("input.txt");

var currentValue = 0;

for (var i = 0; i < lines.Length; i++)
{
    for(var j = 0; j < lines[i].Length; j++)
    {
        if(!char.IsDigit(lines[i][j]) && lines[i][j] != '.')
        {
            if(i != 0)
            {
                if(char.IsDigit(lines[i - 1][j]))
                    currentValue += GetNumber(lines[i - 1], j, j);
                else 
                {
                    if(j != 0 && char.IsDigit(lines[i - 1][j - 1]))
                        currentValue += GetNumber(lines[i - 1], j - 1, j - 1);

                    if(j != lines[i].Length - 1 && char.IsDigit(lines[i - 1][j + 1]))
                        currentValue += GetNumber(lines[i - 1], j + 1, j + 1);
                }
            }

            if(j != 0 && char.IsDigit(lines[i][j - 1]))
                currentValue += GetNumber(lines[i], j - 1, j - 1);

            if(j != lines[i].Length - 1 && char.IsDigit(lines[i][j + 1]))
                currentValue += GetNumber(lines[i], j + 1, j + 1);

            if(i != lines.Length - 1)
            {
                if(char.IsDigit(lines[i + 1][j]))
                    currentValue += GetNumber(lines[i + 1], j, j);
                else
                {
                    if(j != 0 && char.IsDigit(lines[i + 1][j - 1]))
                        currentValue += GetNumber(lines[i + 1], j - 1, j - 1);

                    if(j != lines[i].Length - 1 && char.IsDigit(lines[i + 1][j + 1]))
                        currentValue += GetNumber(lines[i + 1], j + 1, j + 1);
                }
            }
        }
    }
}

Console.WriteLine(currentValue);

int GetNumber(string input, int start, int end)
{
    while(start > 0 && char.IsDigit(input[start - 1]))
        start--;

    while(end < input.Length - 1 && char.IsDigit(input[end + 1]))
        end++;
    
    var result = int.Parse(input.Substring(start, end - start + 1));

    Console.WriteLine($"input {input} result {result}, start {start} end {end}");

    return result;
}