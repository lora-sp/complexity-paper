import os
import pandas as pd
import string

# File path to the corpus samples
input_path = "C:\\Users\\user\\Documents\\Uni\\Master Linguistik Freiburg\\2. Semester\\MorphoSynt Variations\\Term Paper\\coca_five"

# Output file path
output_path = "C:\\Users\\user\\Documents\\Uni\\Master Linguistik Freiburg\\2. Semester\\MorphoSynt Variations\\Term Paper\\wordlist_output"

# Function to check if a word is not punctuation
def is_not_punctuation(word):
    is_punctuation = all(char in punctuation for char in word) 
    return not (is_punctuation) 

# Function to remove the last character if @ or %
def remove_trailing_char(tag):
    if tag.endswith('@') or tag.endswith('%'):
        return tag[:-1]  
    return tag

# Get punctuation
punctuation = set(string.punctuation)

# Read in the files
for file in os.listdir(input_path):
    df = pd.read_csv(
        os.path.join(input_path, file), 
        sep='\t', 
        header=None, 
        names=['ID', 'Word', 'Lowercase', 'Tag'], 
        encoding="latin1", 
        on_bad_lines='skip',
        quoting=csv.QUOTE_NONE
    )

    # Ensure all values in 'Word' and 'Tag' are strings and handle NAs
    df['Word'] = df['Word'].fillna('').astype(str)
    df['Tag'] = df['Tag'].fillna('').astype(str)

    # Filter for words only (no punctuation)
    filtered_df = df[df['Word'].apply(is_not_punctuation)]

    text_counter = 0

    # Loop through each unique value in the 'ID' column and get the first 200 that have over 1000 tokens
    for value in filtered_df['ID'].unique():

        if text_counter >= 200:  
            break

        value_df = filtered_df[filtered_df['ID'] == value]

        if len(value_df) >= 1000:

            sample = value_df.sample(n=1000, random_state=1)

            # Create a new column 'Word_Tag' with the Tag in uppercase
            sample['Word_Tag'] = sample['Word'] + '_' + sample['Tag'].str.upper()

            # Remove @ and % at the end of tags
            sample['Word_Tag'] = sample['Word_Tag'].apply(remove_trailing_char)

            # Write the 'Word_Tag' column to the file
            with open(os.path.join(output_path, "sample_" + os.path.splitext(file)[0] + "_" + str(value) + ".txt"), 'w') as f:  
                for token in sample['Word_Tag']:
                    f.write(token + '\n')
            
            text_counter += 1
            #print(text_counter)

            f.close()

    print(f"Success with file: {file}")
