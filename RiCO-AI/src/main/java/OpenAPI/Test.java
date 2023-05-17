package OpenAPI;

import com.theokanning.openai.completion.CompletionRequest;
import com.theokanning.openai.service.OpenAiService;
import java.util.*;

public class Test {
    public static void run(){
        Scanner sc= new Scanner(System.in);
        System.out.print("Enter a valid token: ");
        String str= sc.nextLine();              //reads string

        OpenAiService service = new OpenAiService(str);
        CompletionRequest completionRequest = CompletionRequest.builder()
                .prompt("Somebody once told me the world is gonna roll me")
                .model("ada")
                .echo(true)
                .build();
        service.createCompletion(completionRequest).getChoices().forEach(System.out::println);
    }
}
