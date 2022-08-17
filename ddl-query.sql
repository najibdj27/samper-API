CREATE TABLE public.user (
    "id" bigserial,
    "first_name" varchar(50) NOT NULL,
    "last_name" varchar(50),
    "date_of_birth" date,
    "username" varchar(20) NOT NULL,
    "email" varchar(50) NOT NULL,
    "phone_number" char(12),
    "password" varchar NOT NULL,
    "created_at" timestamp NOT NULL DEFAULT NOW(),
    "updated_at" timestamp NOT NULL DEFAULT NOW(),
    "created_by" varchar(20) NOT NULL,
    "updated_by" varchar(20) NOT NULL,
    CONSTRAINT "user_pkey" PRIMARY KEY ("id"),
    CONSTRAINT "username_ukey" UNIQUE ("username"),
    CONSTRAINT "email_ukey" UNIQUE ("email"),
    CONSTRAINT "password_ukey" UNIQUE ("password")
);

CREATE TABLE public.student (
    "NIM" bigint,
    "user_id" bigint,
    "face_identity" bytea,
    CONSTRAINT "student_pkey" PRIMARY KEY ("NIM")
);

CREATE TABLE public.lecture (
    "NIP" bigint, -- edit NIP data type
    "user_id" bigint,
    "face_identity" bytea,
    CONSTRAINT "lecture_pkey" PRIMARY KEY ("NIP")
);

CREATE TABLE public.class (
    "id" bigserial,
    "lecture_id" bigint, -- edit lecture_id data type
    "tittle" varchar NOT NULL,
    "subject_id" bigint,
    "created_at" timestamp NOT NULL,
    "updated_at" timestamp NOT NULL,
    "created_by" varchar(20) NOT NULL,
    "updated_by" varchar(20) NOT NULL,
    CONSTRAINT "class_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.presence (
    "id" bigserial,
    "student_id" bigint NOT NULL,
    "class_id" bigint NOT NULL,
    "schedule_id" bigint NOT NULL,
    "check_in" time NOT NULL,
    "check_out" time NOT NULL,
    "location" point NOT NULL,
    "created_at" timestamp NOT NULL,
    "updated_at" timestamp NOT NULL,
    "created_by" varchar(20) NOT NULL,
    "updated_by" varchar(20) NOT NULL,
    CONSTRAINT "presence_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.student_class (
    "id" bigserial,
    "class_id" bigint NOT NULL,
    "student_id" bigint NOT NULL,
    "created_at" timestamp NOT NULL, -- add created_at
    "updated_at" timestamp NOT NULL,
    "created_by" varchar(20) NOT NULL,
    "updated_by" varchar(20) NOT NULL,
    CONSTRAINT "student_class_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.schedule (
    "id" bigserial,
    "class_id" bigint NOT NULL,
    "date" date NOT NULL,
    "time" time NOT NULL,
    "is_active" boolean NOT NULL,
    "created_at" timestamp NOT NULL,
    "updated_at" timestamp NOT NULL,
    "created_by" varchar(20) NOT NULL,
    "updated_by" varchar(20) NOT NULL,
    CONSTRAINT "schedule_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.permission (
    "id" bigserial,
    "schedule_id" bigint NOT NULL,
    "student_id" bigint NOT NULL,
    "detail" varchar NOT NULL,
    "evidence" varchar NOT NULL,
    "permission_status_id" int NOT NULL,
    "created_at" timestamp NOT NULL,
    "updated_at" timestamp NOT NULL,
    "created_by" varchar(20) NOT NULL,
    "updated_by" varchar(20) NOT NULL,
    CONSTRAINT "permission_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.permission_status (
    "id" int,
    "name" varchar NOT NULL,
    CONSTRAINT "permission_status_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.subject (
    "id" int,
    "name" varchar NOT NULL,
    CONSTRAINT "subject_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.role (
    "id" int,
    "name" varchar NOT NULL,
    CONSTRAINT "role_pkey" PRIMARY KEY ("id")
);

CREATE TABLE public.lecture_subject (
    "lecture_id" bigint NOT NULL,
    "subject_id" bigint NOT NULL,
    CONSTRAINT "lecture_subject_pkey" PRIMARY KEY ("lecture_id", "subject_id")
);

CREATE TABLE public.user_role (
    "user_id" bigint NOT NULL,
    "role_id" bigint NOT NULL,
    CONSTRAINT "user_role_pkey" PRIMARY KEY ("user_id", "role_id")
);

ALTER TABLE public.user_role 
ADD CONSTRAINT "user_fkey" FOREIGN KEY ("user_id") REFERENCES public.user("id"),
ADD CONSTRAINT "role_fkey" FOREIGN KEY ("role_id") REFERENCES public.role("id");

ALTER TABLE public.schedule
ADD CONSTRAINT "class_fkey" FOREIGN KEY ("class_id") REFERENCES public.class("id");

ALTER TABLE public.permission 
ADD CONSTRAINT "schedule_fkey" FOREIGN KEY ("schedule_id") REFERENCES public.schedule("id"),
ADD CONSTRAINT "student_fkey" FOREIGN KEY ("student_id") REFERENCES public.student("NIM"),
ADD CONSTRAINT "permission_status_fkey" FOREIGN KEY ("permission_status_id") REFERENCES public.permission_status("id");

ALTER TABLE public.lecture
ADD CONSTRAINT "user_fkey" FOREIGN KEY ("user_id") REFERENCES public.user("id");

ALTER TABLE public.student 
ADD CONSTRAINT "user_fkey" FOREIGN KEY ("user_id") REFERENCES public.user("id");

ALTER TABLE public.class
ADD CONSTRAINT "lecture_fkey" FOREIGN KEY ("lecture_id") REFERENCES public.lecture("NIP"),
ADD CONSTRAINT "subject_fkey" FOREIGN KEY ("subject_id") REFERENCES public.subject("id");

ALTER TABLE public.lecture_subject
ADD CONSTRAINT "lecture_fkey" FOREIGN KEY ("lecture_id") REFERENCES public.lecture("NIP"),
ADD CONSTRAINT "subject_fkey" FOREIGN KEY ("subject_id") REFERENCES public.subject("id");

ALTER TABLE public.student_class
ADD CONSTRAINT "student_fkey" FOREIGN KEY ("student_id") REFERENCES public.student("NIM"),
ADD CONSTRAINT "class_fkey" FOREIGN KEY ("class_id") REFERENCES public.class("id");

ALTER TABLE public.presence
ADD CONSTRAINT "student_fkey" FOREIGN KEY ("student_id") REFERENCES public.student("NIM"),
ADD CONSTRAINT "class_fkey" FOREIGN KEY ("class_id") REFERENCES public.class("id"),
ADD CONSTRAINT "schedule_fkey" FOREIGN KEY ("schedule_id") REFERENCES public.schedule("id");