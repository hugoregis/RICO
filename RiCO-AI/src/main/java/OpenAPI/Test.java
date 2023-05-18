package OpenAPI;

import com.theokanning.openai.completion.CompletionRequest;
import com.theokanning.openai.completion.chat.ChatCompletionRequest;
import com.theokanning.openai.completion.chat.ChatMessage;
import com.theokanning.openai.completion.chat.ChatMessageRole;
import com.theokanning.openai.service.OpenAiService;
import java.util.*;

public class Test {
    public static void run(){
        Scanner sc= new Scanner(System.in);
        System.out.print("Enter a valid token: ");
        String str= sc.nextLine();              //reads string

        OpenAiService service = new OpenAiService(str);

        System.out.println("Creating chat completion request...");
        final List<ChatMessage> messages = new ArrayList<>();
        final ChatMessage systemMessage = new ChatMessage(ChatMessageRole.SYSTEM.value(),
                "Predicate logic expresses natural language propositions into the following format: predicate(arguments), where 'arguments' can be composed of one or multiple objects. \n" +
                        "\n" +
                        "For example, 'birds fly' will be formalized as 'fly(bird)' in predicate logic. Similarly 'Socrates is a man' will be formalized as 'man(socrates)'. A predicate can also take multiple arguments if it is a relation. For instance 'John likes Mary' will be formalized as 'likes(john, mary)' and 'John gives a book to Mary' as 'gives(john, Mary, book)'.\n" +
                        "\n" +
                        "Now, assuming a certain predicate logic framework, how would you convert the following list of predicate logic expressions into a list of natural language expressions? And for each expression, specify if it would be commonly considered as a true proposition or not.\n" +
                        "[lay_eggs(penguin), lay_eggs(shark), class(dog, mammal), class(bird), class(herring, fish), homeothermic(woodpecker), fly(hawk), fly(penguin), fly(bat)]"
                );
        messages.add(systemMessage);
        ChatCompletionRequest chatCompletionRequest = ChatCompletionRequest
                .builder()
                .model("gpt-3.5-turbo")
                .messages(messages)
                .n(1)
                .maxTokens(200)
                .logitBias(new HashMap<>())
                .build();

        System.out.println("Generating chat completion...");

        System.out.println("\n\n");
        System.out.println(service.createChatCompletion(chatCompletionRequest).toString());

        service.shutdownExecutor();
    }
}
