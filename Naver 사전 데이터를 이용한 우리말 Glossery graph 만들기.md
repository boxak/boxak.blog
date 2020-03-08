# Naver 사전 데이터를 이용한 우리말 Glossary graph 만들기 

### 차례

- 개요

- DICT.csv 파일에서 어간 추출 -> PosList
- PosList를 Graph 형태로 바꾸고 의미번호 붙이기
- 관련어 데이터를 활용해 각 단어의 subgraph 노드 개수 구하기
- 네이버 사전 크롤링의 속도 향상 방법에 관해



## 개요

이번 프로젝트에 대해 간략히 요약하자면 약 20만개의 단어의 뜻풀이를 가지고 (단어, 단어) 쌍으로 이루어진 그래프를 만들고 그래프의 정보를 추출하는 과정이라고 할 수 있다. 

![image-20200216171549932.png](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216171549932.png?raw=true)



## DICT.csv 파일에서 어간 추출

- RcppMeCab을 활용한 한국어 형태소 분석

```r
setwd('C:\\Users\\82102\\Documents\\workingDIR\\function')
source('readcsv.R',encoding = 'UTF-8')
data <- readcsv('DICT.csv')
data[data$'의미어'=='인간001',] ###'의미어' 컬럼이 '인간001'인 행만 추출한다.
```

![image-20200216180754722](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216180754722.png?raw=true)

```r
RcppMeCab::pos(data[data$'의미어'=='인간001',][[6]],format = 'data.frame')
```

![image-20200216180912181](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216180912181.png?raw=true)

위와 같이 RcppMeCab을 활용하면 문장을 단어별로 쪼개는 tokenize 기능과 함께 단어별로 어떤 품사인지 label을 붙여준다. 여기서 우리가 활용할 품사는 NNG(일반명사), NNP(고유명사), VV(동사원형),

VV+ 꼴의 품사들만 추출할 것이다.



명사의 경우, 명사가 활용되어 형태가 바뀌는 경우는 거의 없으므로 그대로 추출한다.

동사원형, VV의 경우에는 어간이 보존되어 있어 맨 끝에 '다'만 붙이면 복원이 된다.

ex) 먹 -> 먹다, 잡 -> 잡다.



예를 들어, '잡는 사람', '먹을 음식'을 보면, 

![image-20200216203941890](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216203941890.png?raw=true)

동사를 어간과 어미로 분리해준다. 그래서 뒤에 '다'만 붙이면 먹다, 잡다 로 복원시킬 수 있다.

반면에, '움직일 타이밍'의 경우,

![image-20200216204432204](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216204432204.png?raw=true)

위와 같이 움직일에 VV+ETM 으로 붙어있는 것을 볼 수 있다. 아마도 'ㄹ'이 관형형 전성 어미에 해당하는 듯하다. 

![image-20200216204720746](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216204720746.png?raw=true)

어간이 변형되어있을 경우에 이걸 어떻게 원형을 찾을까 고민하다가 네이버 사전 데이터를 한번 이용해 보기로 했다. 그래서 이 경우엔 findStem이라는 함수를 거치도록 코드를 짰다.

```r
findStem <- function(str){
  url <- paste0("https://dict.naver.com/search.nhn?dicQuery=",URLencode(str),"&query=",URLencode(str),"&target=dic&ie=utf8&query_utf=&isOnlyViewEE=")
  html <- read_html(url)
  item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(1) > a:nth-child(1) > span > strong')
  text <- html_text(item,trim = TRUE) ## text에 네이버 검색으로 찾은 페이지의 단어를 넣는다.
  ## 만일 '다'로 끝나는 text가 추출되지 않으면 될때까지 inx를 증가
  inx <- 1
  while(is.na(text) | stringr::str_sub(text,nchar(text),nchar(text))!="다"){
    inx <- inx+1
    cssPath = paste0('#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(',as.character(inx),') > p:nth-child(1) > a:nth-child(1) > span > strong')
    item <- html_node(html,css = cssPath)
    text <- html_text(item,trim = TRUE)
    if(inx>10) break
  }
  ## 만일 '다'로 끝나는 text가 추출되지 않으면 영어사전에서 찾는다.
  inx <- 1
  while(is.na(text) | stringr::str_sub(text,nchar(text),nchar(text))!="다"){
    cssPath = paste0('#content > div.en_dic_section.search_result.dic_en_entry > dl > dt:nth-child(',as.character(inx),') > a:nth-child(1) > span > strong')
    item <- html_node(html,css = cssPath)
    text <- html_text(item,trim = TRUE)
    inx <- inx+1
    if(inx>10) break
  }

  return(text)
}
```

![image-20200216010841574](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216010841574.png?raw=true)

![image-20200216010929632](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216010929632.png?raw=true)

'사는'의 검색 결과 '사는'이 가장 먼저 검색된다.

'사는'의 seletor 를 copy해서 httr 패키지의 함수를 이용해 접근할 수 있다.

-> #content > div.kr_dic_section.search_result.dic_kr_entry > ul > **li:nth-child(1)** > p:nth-child(1) > a > span > strong

그러나 '사는'은 원형이 아니므로, ('다'로 끝나지 않기 때문에) 다음 검색 결과로 넘어간다.



'사는'은 '살다' 혹은 '사다'의 변형일 수 있는데, '살다'의 활용형이면 '사다'라는 텍스트를 가지고 오는 것은 잘못된 것일 수 있다. 하지만 그걸 구분하지 않고 '다'로 끝나는 텍스트를 데리고 온다.

'사다'의 selector는 

#content > div.kr_dic_section.search_result.dic_kr_entry > ul > **li:nth-child(2)** > p:nth-child(1) > a:nth-child(1) > span > strong

li라는 selector를 보면, nth-child값이 각각 1과 2이므로 li:nth-child(m) 이 들어가면 m번째 검색결과가 나오게 됨을 볼 수 있다. 각 단어마다 selector의 a태그에 그냥 a로 들어가는 것도 있고 a:nth-child(1) 로 들어가는 것도 있지만 둘다 동일한 텍스트로 접근을 하는 것을 확인하였다.

그러므로 while문으로 반복을 시켜서 text에 '다'로 끝나는 단어가 담기지 않았으면 

li:nth-child(m) 에서 m을 1증가시킨 li:nth-child(m+1)을 가지고 있는 selector에 접근해 text를 확인한다. '다'로 끝나는 단어이면 결과를 반환한다. 이 과정에서 '다'로 끝나는 단어를 10번 이내에 찾지 못하면 findStem에서 NA를 반환하도록 하였다. 즉, 이 데이터는 나중에 걸러야 한다.

이 작업이 끝난 후, PosList_medium.csv 라는 이름으로 저장하였다.

```r
source('writecsv.R',encoding = 'UTF-8')
PosTable <- data.frame(PosList)
writecsv(PosTable,'PosList_medium.csv')
```





## PosList를 Graph형태로 바꾸고 의미번호 붙이기

![image-20200216205635137](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216205635137.png?raw=true)

PosList_medium 의 데이터를 살펴보면 위와 같이 되어있다. 여기에 NA(결측치)가 섞여있다.

```r
PosList <- na.omit(PosList)
```

na.omit을 사용해 결측치가 있는 데이터를 제거할 수 있다. 그리고 나서, 의미어 목록과 뜻풀이에서 추출한 어간 목록을 합친다.

![image-20200216211850859](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216211850859.png?raw=true)

이제 wordList에 의미번호를 붙이는 일만 남았다. 이 경우, 각 단어간 거리(0은 같은 의미어라는 의미.

1이 두 단어간 가질 수 있는 가장 가까운 거리이다. 즉, 1에 가까울 수록 유사한 단어)에 따라 번호를 매기는 것이 정석적인 방법이다. 즉, 가001 과 거리가 제일 가까운 것이 경계001 ~ 경계004 중 경계004가 가장 가까우면 '경계'+'004'로 만들어주는 것이다. 위 데이터는 'graph_medium.csv'라는 이름으로 저장했다.

하지만 이번에도 네이버 사전을 참조하는 방식으로 진행했다. 

![image-20200216221102994](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216221102994.png?raw=true)

경계-4 부터 7,3,8,6이 있는 것을 볼 수 있다. 우리가 흔히 쓰는 경계의 의미는 경계004가 가장 많이 사용되어서 '경계'를 검색했을 때, 가장 위에 뜨는 것으로 볼 수 있다.



![image-20200216221355312](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216221355312.png?raw=true)

숫자 4에 해당하는 selector를 추출하면

#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(1) > sup

이 추출되는데, '움직이다'와 같이 '움직이다002'가 존재하지 않는 경우는, 저 번호가 붙어있지 않아서

추출하면 na가 될 것이다. 이 경우는 001을 붙이도록 만들었다.

```r
findSup <- function(str){
  url <- paste0("https://dict.naver.com/search.nhn?dicQuery=",URLencode(str)             "&query=",URLencode(str),"&target=dic&ie=utf8&query_utf=&isOnlyViewEE=")
  html <- read_html(url)
  item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(1) > sup')
  supNumber <- html_text(item,trim = TRUE)
  ### supNumber에 추출된 번호를 담는다.
  if(!is.na(supNumber)){ ### 만일, supNumber에 숫자가 담겨졌으면.
    if(as.integer(supNumber)<10){ ### supNumber 가 1 ~ 9까지의 숫자이면 00x가 되도록
      str <- paste0(str,'00',supNumber)
    }
    if(as.integer(supNumber)>=10){ ### supNumber가 두자리 수이면 0xx가 되도록
      str <- paste0(str,'0',supNumber)
    }
  }
  if(is.na(supNumber)){ ### 만일 supNumber가 NA이면 원래 str에 001을 뒤에 붙인다.
    str <- paste0(str,'001')
  }
  return(str)
}
```

이를 반복하여 graph_medium2.csv로 저장하였고 다음과 같다.

![image-20200216223157540](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216223157540.png?raw=true)





## 관련어 데이터를 활용해 각 단어의 subgraph 노드 개수 구하기

이번에는 EZmeta30.csv 라는 파일을 사용할 것이다.

```r
ezdata <- readcsv('/original data/EZmeta30.csv')
head(ezdata)
```

![image-20200216231349049](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216231349049.png?raw=true)

위와 같이 (표제어,관련어) 쌍으로 이루어진 데이터임을 확인할 수 있다. 우리가 관심있는 정보는 

(head, tail)로 이뤄진 그래프이므로 표제어, 의미어만 의미번호를 각각 붙여서 추출한다.



```r
### relData에 ezdata에서 표제어와 비슷한말, 반대말 관계인 행만 담는다.
relData <- ezdata[ezdata$relation2=='비슷한말' | ezdata$relation2=='반대말',]
### -,^ 과 같은 특수문자는 ㄱ-힣에 포함되는 문자가 아니므로 
### gsub함수에서 제거된다.
refine <- gsub('[^ㄱ-힣]','',relData$'표제어')
### 특수문자 제거한 표제어 + 의미번호를 붙여준다.
head <- paste0(refine,relData$'의미번호')
### 관련어도 마찬가지 작업을 해준다.
refine2 <- gsub('[^ㄱ-힣]','',relData$'관련어')
tail <- paste0(refine2,relData$'의미번호_1')
### head, tail을 가로로 붙여준다.
refinedData <- cbind(head,tail) 
head(refinedData)
```

![image-20200216232258006](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200216232258006.png?raw=true)



refineData 를 가지고 dfs(depth first search, 깊이 우선 탐색)을 진행한다. 방법은 이렇다.

1. 각 단어에 번호 부여해주는 작업

```r
wordList <- c('가톨릭002','서학002','가톨릭교001','공교002','가톨릭교001','천주학001',...)
```

먼저 wordList에 head, tail에 있는 모든 글자를 다 집어넣는다. 이때는 가나다 순 고려하지 않고, 중복도 제거하지 않았기 때문에 wordList에 있는 데이터 그대로 쓸 수 없다.

```r
uniqueSort(wordList) ### 가나다 순으로 정렬하고 중복을 제거한다.
```

물론 uniqueSort 라는 함수는 R에 내장되어 있지 않다. 자바로 구현했는데, 어느 정도는 pseudo code를 사용하며 설명하겠다. 그냥 가나다 순으로 정렬하는 것은 자바에 내장된 메서드를 쓰면 된다.

이를 테면, (가톨릭, 가톨릭, 서학, 천주교, 기독교, 개신교, 개신교) 라는 데이터가 wordList에 담겼다고 하면

```r
wordList : '가톨릭','가톨릭','서학','천주교','기독교','개신교','개신교'
sort(wordList) : '가톨릭','가톨릭','개신교','개신교','서학','천주교'
keyList : -> 빈 리스트
inx = 1
keyList.push(wordList[1]) ### 첫번째 단어는 무조건 unique하므로 담는다.
for(i=2 ~ length(wordList)){
    if(wordList[i]!=wordList[i-1]){
        keyList.push(wordList[i]) ### 앞 단어와 다르면 무조건 unique하므로 담는다.
    }
}
```

이렇게 하면, refinedData의 모든 노드들이 unique하게 keyList 라는 리스트에 담기게 된다.

이제 각 단어별로 keyList에 몇 번째에 위치하느냐로 번호를 매길 것이다. 

refinedData와 같은 꼴의 2차원 정수형 배열 numGraph를 만든다.



```r
for(i=1 ~ number_of_row(refinedData)){
    numGraph[i][1] = binarySearch(keyList,refinedData[i][1]) // -> head
    numGraph[i][2] = binarySearch(keyList,refinedData[i][2]) // -> tail
}
```

binarySearch를 사용하는 이유는, keyList의 맨 앞에서 하나하나 찾아보며 일치하는지 보는 것보다 훨씬 시간이 많이 단축되기 때문이다. 데이터 크기가 N일때, 시간복잡도는 log(N)이므로 굉장히 빠르게 탐색할 수 있다.

이제 DFS에 사용할 수 있는 numGraph를 만들었다. keyList의 길이를 keyLen이라 하자.

그러면, index = 1부터 keyLen까지 하나하나 DFS를 진행한다.

```pseudocode
cntVisit ###방문 횟수 저장하는 변수
visited[keyLen] ###방문 정보 저장하는 boolean형 배열
cntArr[keyLen] ### 각 노드 별로 DFS로 몇 개 노드를 방문했는지 저장하는 int형 배열
for(i=1 ~ keyLen){
	for(j=1 ~ keyLen){
		visited[keyLen] = false ### 매 DFS를 시작하기 전, visited를 false로 초기화.
	}
	cntVisit = 0 ### 방문 횟수도 0으로 초기화.
	dfs(vector,visited,i) ### vector의 n번째에는 n번째 노드와 연결된 노드 번호들이 담겨							###	있다. 
	cntArr[i] = cntVisit ### 방문이 끝나면 cntArr에 cntVisit을 넣어준다.
}

void dfs(vector,visited,node){
	visited[node] = true ### 각 노드 방문마다 해당 노드의 visited를 true로 만든다.
	### 즉, 방문한 적이 있으면 visited에 기록한다.
	cntVisit = cntVisit+1 ### cntVisit도 1씩 증가시킨다.
	for(int next : vector[node]){ ### vector[node]에 담긴 변수 next
		if(visited[next]==false){ ### 이번 DFS에서 방문한 적이 없으면
			dfs(vector,visited,next) ### 방문. 즉, 이미 방문한 적있으면 스킵
		}
	}
}
```

다음과 같은 데이터를 얻을 수 있다.

![image-20200217010044603](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200217010044603.png?raw=true)



이런 시행을 상위어 관계, 하위어 관계 데이터에서도 할 수 있는데, 편의를 위해 상위어 데이터에서는 head 와 tail을 위치를 바꾸었다. 즉, 인위적으로 하위어 관계 데이터로 만들고 DFS를 진행하였다.







## 네이버 사전 크롤링의 속도 향상 방법에 관해

PosList 데이터의 행은 358385개인데, 358585개의 단어를 모두 네이버 사전에서 크롤링해서 끌고 오는 방식은 상당히 시간이 많이 걸린다. 그래서 시행한 방법은 굉장히 단순한데, 원격 rstudio로 접속할 수 있는 user 계정을 6개 정도 만든 다음에 358385/6 = 59730개 씩 나누어서 (user6은 59735개)

코드를 실행시키는 것이다. 다만, 만일 크롬에서 탭을 6개 열어놓고 하면 접속되지 않는다. 한 계정이 접속하면 다른 계정의 접속이 끊어지는데, 이는 chrome, Microsoft Edge, IE, Opera, Brave, FireFox 의 

6개 브라우저로 동시 접속함으로써 해결할 수 있었다.(맨 밑의 Browser는 6개의 브라우저의 총 데이터 처리에 대해 나타낸 것이다.)

![image-20200217015845792](https://github.com/boxak/boxak.github.io/blob/master/assets/typora-user-images/image-20200217015845792.png?raw=true)

각각의 처리 속도를 보면 로컬 컴퓨터로 실행시킨 rstudio의 속도가 더 빠르지만, 전체 브라우저의 데이터 처리 속도가 약 5.4배 빠른 것을 확인할 수 있다.