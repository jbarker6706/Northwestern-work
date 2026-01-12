# -*- coding: utf-8 -*-
"""
Created on Sun Dec  8 14:45:24 2019

@author: jbark
"""

r""" Build character sequence-to-sequence training set """
import pandas as pd

with open("movies-sequence-input.txt", "r") as file_input:
    movie_input = file_input.read()
df_input = pd.DataFrame(movie_input.split('\n'),columns=list('i'))
df_input = df_input.fillna(' ')

with open("movies-sequence-output.txt", "r") as file_output:
    movie_output = file_output.read()
df_output = pd.DataFrame(movie_output.split('\n'),columns=list('o'))
df_output = df_output.fillna(' ')

input_texts, target_texts = [], []
input_vocabulary = set()
output_vocabulary = set()
start_token = '\t'
stop_token = '\n'
#max_training_samples = min(25000, len(df_input) - 1)
max_training_samples = min(10000, len(df_input) - 1)

#Build the vocabulary
for input_text, target_text in zip(df_input.i, df_output.o):
    target_text = start_token + target_text \
        + stop_token  # <5>
    if(len(input_text)>400):
        input_texts.append(input_text[:400])
    else:
        input_texts.append(input_text)
    if(len(target_text)>400):
        target_texts.append(target_text[:400])
    else:
        target_texts.append(target_text)
    for char in input_text:  # <6>
        if char not in input_vocabulary:
            input_vocabulary.add(char)
    for char in target_text:
        if char not in output_vocabulary:
            output_vocabulary.add(char)

input_vocabulary = sorted(input_vocabulary)
output_vocabulary = sorted(output_vocabulary)

input_vocab_size = len(input_vocabulary)
output_vocab_size = len(output_vocabulary)

max_encoder_seq_len = df_input.i.str.len().max()
max_decoder_seq_len = df_output.o.str.len().max()

input_token_index = dict([(char, i) for i, char in enumerate(input_vocabulary)])

target_token_index = dict([(char, i) for i, char in enumerate(output_vocabulary)])

reverse_input_char_index = dict((i, char) for char, i in input_token_index.items())
reverse_target_char_index = dict((i, char) for char, i in target_token_index.items())

max_encoder_seq_len = df_input.i.str.len().max()
max_decoder_seq_len = df_output.o.str.len().max()

#Build the data structures for input to the encoder decoder architecture
import numpy as np

encoder_input_data = np.zeros(
    (len(input_texts), max_encoder_seq_len, input_vocab_size),
    dtype='float32')
decoder_input_data = np.zeros(
    (len(input_texts), max_decoder_seq_len, output_vocab_size),
    dtype='float32')
decoder_target_data = np.zeros(
    (len(input_texts), max_decoder_seq_len, output_vocab_size),
    dtype='float32')

for i, (input_text, target_text) in enumerate(
        zip(input_texts, target_texts)):
    for t, char in enumerate(input_text):
        encoder_input_data[
            i, t, input_token_index[char]] = 1.
    for t, char in enumerate(target_text):
        decoder_input_data[
            i, t, target_token_index[char]] = 1.
        if t > 0:
            decoder_target_data[i, t - 1, target_token_index[char]] = 1

#Build and train the model
            from keras.models import Model
from keras.layers import Input, LSTM, Dense

batch_size = 64
epochs = 100
num_neurons = 256

encoder_inputs = Input(shape=(None, input_vocab_size))
encoder = LSTM(num_neurons, return_state=True)
encoder_outputs, state_h, state_c = encoder(encoder_inputs)
encoder_states = [state_h, state_c]

decoder_inputs = Input(shape=(None, output_vocab_size))
decoder_lstm = LSTM(num_neurons, return_sequences=True,
                    return_state=True)
decoder_outputs, _, _ = decoder_lstm(decoder_inputs,
                                     initial_state=encoder_states)
decoder_dense = Dense(output_vocab_size, activation='softmax')
decoder_outputs = decoder_dense(decoder_outputs)
model = Model([encoder_inputs, decoder_inputs], decoder_outputs)

model.compile(optimizer='rmsprop', loss='categorical_crossentropy',
              metrics=['acc'])
model.fit([encoder_input_data, decoder_input_data],
          decoder_target_data, batch_size=batch_size, epochs=epochs,
          validation_split=0.1)

#Build chatbot and start chatting
encoder_model = Model(encoder_inputs, encoder_states)
thought_input = [
    Input(shape=(num_neurons,)), Input(shape=(num_neurons,))]
decoder_outputs, state_h, state_c = decoder_lstm(
    decoder_inputs, initial_state=thought_input)
decoder_states = [state_h, state_c]
decoder_outputs = decoder_dense(decoder_outputs)

decoder_model = Model(
    inputs=[decoder_inputs] + thought_input,
    output=[decoder_outputs] + decoder_states)

def decode_sequence(input_seq):
    thought = encoder_model.predict(input_seq)  # <1>

    target_seq = np.zeros((1, 1, len(output_vocabulary)))  # <2>
    target_seq[0, 0, output_vocabulary.index(stop_token)
        ] = 1.  # <3>
    stop_condition = False
    generated_sequence = ''

    while not stop_condition:
        output_tokens, h, c = decoder_model.predict(
            [target_seq] + thought) # <4>

        generated_token_idx = np.argmax(output_tokens[0, -1, :])
        generated_char = output_vocabulary[generated_token_idx]
        generated_sequence += generated_char
        if (generated_char == stop_token or
                len(generated_sequence) > max_decoder_seq_len
                ):  # <5>
            stop_condition = True

        target_seq = np.zeros((1, 1, len(output_vocabulary)))  # <6>
        target_seq[0, 0, generated_token_idx] = 1.
        thought = [h, c]  # <7>

    return generated_sequence

def respond(input_text):
    input_text = input_text.lower()
    input_text = ''.join(c if c in input_vocabulary else ' ' for c in input_text)
    input_seq = np.zeros((1, max_encoder_seq_len, len(input_vocabulary)), dtype='float32')
    for t, c in enumerate(input_text):
        input_seq[0, t, input_vocabulary.index(c)] = 1.
    decoded_sentence = decode_sequence(input_seq)
    print('Human: {}'.format(input_text))
    print('Bot:', decoded_sentence)
    return decoded_sentence
